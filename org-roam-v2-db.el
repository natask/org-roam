;;; org-roam-v2-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0") (magit-section "2.90.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides the underlying database api to org-roam.
;;
;;; Code:
;;;; Library Requires
(eval-when-compile (require 'subr-x))
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'seq)

(eval-and-compile
  (require 'org-roam-v2-macs)
  ;; For `org-with-wide-buffer'
  (require 'org-macs))
(require 'org)
(require 'ol)
(require 'org-roam-v2-utils)

(defvar org-roam-v2-find-file-hook)
(defvar org-roam-v2-directory)
(defvar org-roam-v2-verbose)
(defvar org-agenda-files)

(declare-function org-roam-v2-id-at-point "org-roam-v2")
(declare-function org-roam-v2--list-all-files "org-roam-v2")
(declare-function org-roam-v2-node-at-point "org-roam-v2")

;;;; Options
(defcustom org-roam-v2-db-location (expand-file-name "org-roam-v2.db" user-emacs-directory)
  "The full path to file where the org-roam-v2 database is stored.
If this is non-nil, the org-roam-v2 sqlite database is saved here.

It is the user's responsibility to set this correctly, especially
when used with multiple org-roam-v2 instances."
  :type 'string
  :group 'org-roam-v2)

(defcustom org-roam-v2-db-gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During large, heavy operations like `org-roam-v2-db-sync',
many GC operations happen because of the large number of
temporary structures generated (e.g. parsed ASTs). Temporarily
increasing `gc-cons-threshold' will help reduce the number of GC
operations, at the cost of temporary memory usage.

This defaults to the original value of `gc-cons-threshold', but
tweaking this number may lead to better overall performance. For
example, to reduce the number of GCs, one may set it to a large
value like `most-positive-fixnum'."
  :type 'int
  :group 'org-roam-v2)

(defconst org-roam-v2-db-version 16)
(defconst org-roam-v2--sqlite-available-p
  (with-demoted-errors "org-roam-v2 initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar org-roam-v2-db--connection (make-hash-table :test #'equal)
  "Database connection to org-roam-v2 database.")

;;;; Core Functions

(defun org-roam-v2-db--get-connection ()
  "Return the database connection, if any."
  (gethash (expand-file-name org-roam-v2-directory)
           org-roam-v2-db--connection))

(defun org-roam-v2-db ()
  "Entrypoint to the org-roam-v2 sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (org-roam-v2-db--get-connection)
               (emacsql-live-p (org-roam-v2-db--get-connection)))
    (let ((init-db (not (file-exists-p org-roam-v2-db-location))))
      (make-directory (file-name-directory org-roam-v2-db-location) t)
      (let ((conn (emacsql-sqlite org-roam-v2-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (expand-file-name org-roam-v2-directory)
                 conn
                 org-roam-v2-db--connection)
        (when init-db
          (org-roam-v2-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam-v2-db--upgrade-maybe conn version)))
          (cond
           ((> version org-roam-v2-db-version)
            (emacsql-close conn)
            (user-error
             "The org-roam-v2 database was created with a newer org-roam-v2 version.  "
             "You need to update the org-roam-v2 package"))
           ((< version org-roam-v2-db-version)
            (emacsql-close conn)
            (error "BUG: The org-roam-v2 database scheme changed %s"
                   "and there is no upgrade path")))))))
  (org-roam-v2-db--get-connection))

;;;; Entrypoint: (org-roam-v2-db-query)
(define-error 'emacsql-constraint "SQL constraint violation")
(defun org-roam-v2-db-query (sql &rest args)
  "Run SQL query on org-roam-v2 database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (apply #'emacsql (org-roam-v2-db) sql args))

(defun org-roam-v2-db-query! (handler sql &rest args)
  "Run SQL query on org-roam-v2 database with ARGS.
SQL can be either the emacsql vector representation, or a string.
The query is expected to be able to fail, in this situation, run HANDLER."
  (condition-case err
      (org-roam-v2-db-query sql args)
    (emacsql-constraint
     (funcall handler err))))

;;;; Schemata
(defconst org-roam-v2-db--table-schemata
  '((files
     [(file :unique :primary-key)
      (hash :not-null)
      (atime :not-null)
      (mtime :not-null)])

    (nodes
     ([(id :not-null :primary-key)
       (file :not-null)
       (level :not-null)
       (pos :not-null)
       todo
       priority
       (scheduled text)
       (deadline text)
       title
       properties
       olp]
      (:foreign-key [file] :references files [file] :on-delete :cascade)))

    (aliases
     ([(node-id :not-null)
       alias]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (refs
     ([(node-id :not-null)
       (ref :not-null)
       (type :not-null)]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (tags
     ([(node-id :not-null)
       tag]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (links
     ([(pos :not-null)
       (source :not-null)
       (dest :not-null)
       (type :not-null)
       (properties :not-null)]
      (:foreign-key [source] :references nodes [id] :on-delete :cascade)))))

(defconst org-roam-v2-db--table-indices
  '((alias-node-id aliases [node-id])
    (refs-node-id refs [node-id])
    (tags-node-id tags [node-id])))

(defun org-roam-v2-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (emacsql db "PRAGMA foreign_keys = ON")
    (pcase-dolist (`(,table ,schema) org-roam-v2-db--table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (pcase-dolist (`(,index-name ,table ,columns) org-roam-v2-db--table-indices)
      (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
    (emacsql db (format "PRAGMA user_version = %s" org-roam-v2-db-version))))

(defun org-roam-v2-db--upgrade-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version org-roam-v2-db-version)
        (progn
          (org-roam-v2-message (format "Upgrading the org-roam-v2 database from version %d to version %d"
                                    version org-roam-v2-db-version))
          (org-roam-v2-db-sync t))))
  version)

(defun org-roam-v2-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-roam-v2-directory'."
  (unless db
    (setq db (org-roam-v2-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun org-roam-v2-db--close-all ()
  "Closes all database connections made by org-roam-v2."
  (dolist (conn (hash-table-values org-roam-v2-db--connection))
    (org-roam-v2-db--close conn)))

;;;; Database API
;;;;; Clearing
(defun org-roam-v2-db-clear-all ()
  "Clears all entries in the org-roam-v2 cache."
  (interactive)
  (when (file-exists-p org-roam-v2-db-location)
    (dolist (table (mapcar #'car org-roam-v2-db--table-schemata))
      (org-roam-v2-db-query `[:delete :from ,table]))))

(defun org-roam-v2-db-clear-file (&optional file)
  "Remove any related links to the FILE.
This is equivalent to removing the node from the graph.
If FILE is nil, clear the current buffer."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (org-roam-v2-db-query [:delete :from files
                      :where (= file $s1)]
                     file))

;;;;; Updating tables
(defun org-roam-v2-db-insert-file ()
  "Update the files table for the current buffer.
If UPDATE-P is non-nil, first remove the file in the database."
  (let* ((file (buffer-file-name))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (org-roam-v2-db--file-hash)))
    (org-roam-v2-db-query
     [:insert :into files
      :values $v1]
     (list (vector file hash atime mtime)))))

(defun org-roam-v2-db-get-scheduled-time ()
  "Return the scheduled time at point in ISO8601 format."
  (when-let ((time (org-get-scheduled-time (point))))
    (org-format-time-string "%FT%T%z" time)))

(defun org-roam-v2-db-get-deadline-time ()
  "Return the deadline time at point in ISO8601 format."
  (when-let ((time (org-get-deadline-time (point))))
    (org-format-time-string "%FT%T%z" time)))

(defun org-roam-v2-db-map-headlines (fns)
  "Run FNS over all headlines in the current buffer."
  (org-with-point-at 1
    (org-map-entries
     (lambda ()
       (dolist (fn fns)
         (funcall fn))))))

(defun org-roam-v2-db-map-links (fns)
  "Run FNS over all links in the current buffer."
  (org-with-point-at 1
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (dolist (fn fns)
          (funcall fn link))))))

(defun org-roam-v2-db-insert-file-node ()
  "Insert the file-level node into the org-roam-v2 cache."
  (org-with-point-at 1
    (when (= (org-outline-level) 0)
      (when-let ((id (org-id-get)))
        (let* ((file (buffer-file-name (buffer-base-buffer)))
               (title (org-link-display-format
                       (or (cadr (assoc "TITLE" (org-collect-keywords '("title"))
                                        #'string-equal))
                           (file-relative-name file org-roam-v2-directory))))
               (pos (point))
               (todo nil)
               (priority nil)
               (scheduled nil)
               (deadline nil)
               (level 0)
               (aliases (org-entry-get (point) "ROAM_ALIASES"))
               (tags org-file-tags)
               (refs (org-entry-get (point) "ROAM_REFS"))
               (properties (org-entry-properties))
               (olp (org-get-outline-path)))
          (org-roam-v2-db-query!
           (lambda (err)
             (lwarn 'org-roam-v2 :warning "%s for %s (%s) in %s"
                    (error-message-string err)
                    title id file))
           [:insert :into nodes
            :values $v1]
           (vector id file level pos todo priority
                   scheduled deadline title properties olp))
          (when tags
            (org-roam-v2-db-query
             [:insert :into tags
              :values $v1]
             (mapcar (lambda (tag)
                       (vector id (substring-no-properties tag)))
                     tags)))
          (when aliases
            (org-roam-v2-db-query
             [:insert :into aliases
              :values $v1]
             (mapcar (lambda (alias)
                       (vector id alias))
                     (split-string-and-unquote aliases))))
          (when refs
            (setq refs (split-string-and-unquote refs))
            (let (rows)
              (dolist (ref refs)
                (if (string-match org-link-plain-re ref)
                    (progn
                      (push (vector id (match-string 2 ref)
                                    (match-string 1 ref)) rows))
                  (lwarn '(org-roam-v2) :warning
                         "%s:%s\tInvalid ref %s, skipping..."
                         (buffer-file-name) (point) ref)))
              (when rows
                (org-roam-v2-db-query
                 [:insert :into refs
                  :values $v1]
                 rows)))))))))

(defun org-roam-v2-db-insert-node-data ()
  "Insert node data for headline at point into the org-roam-v2 cache."
  (when-let ((id (org-id-get)))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (heading-components (org-heading-components))
           (pos (point))
           (todo (nth 2 heading-components))
           (priority (nth 3 heading-components))
           (level (nth 1 heading-components))
           (scheduled (org-roam-v2-db-get-scheduled-time))
           (deadline (org-roam-v2-db-get-deadline-time))
           (title (org-link-display-format (nth 4 heading-components)))
           (properties (org-entry-properties))
           (olp (org-get-outline-path)))
      (org-roam-v2-db-query!
       (lambda (err)
         (lwarn 'org-roam-v2 :warning "%s for %s (%s) in %s"
                (error-message-string err)
                title id file))
       [:insert :into nodes
        :values $v1]
       (vector id file level pos todo priority
               scheduled deadline title properties olp)))))

(defun org-roam-v2-db-insert-aliases ()
  "Insert aliases for node at point into org-roam-v2 cache."
  (when-let ((node-id (org-id-get))
             (aliases (org-entry-get (point) "ROAM_ALIASES")))
    (org-roam-v2-db-query [:insert :into aliases
                        :values $v1]
                       (mapcar (lambda (alias)
                                 (vector node-id alias))
                               (split-string-and-unquote aliases)))))

(defun org-roam-v2-db-insert-tags ()
  "Insert tags for node at point into org-roam-v2 cache."
  (when-let ((node-id (org-id-get))
             (tags (org-get-tags)))
    (org-roam-v2-db-query [:insert :into tags
                        :values $v1]
                       (mapcar (lambda (tag)
                                 (vector node-id (substring-no-properties tag))) tags))))

(defun org-roam-v2-db-insert-refs ()
  "Insert refs for node at point into org-roam-v2 cache."
  (when-let* ((node-id (org-id-get))
              (refs (org-entry-get (point) "ROAM_REFS"))
              (refs (split-string-and-unquote refs)))
    (let (rows)
      (dolist (ref refs)
        (save-match-data
          (if (string-match org-link-plain-re ref)
              (progn
                (push (vector node-id (match-string 2 ref) (match-string 1 ref)) rows))
            (lwarn '(org-roam-v2) :warning
                   "%s:%s\tInvalid ref %s, skipping..." (buffer-file-name) (point) ref))))
      (org-roam-v2-db-query [:insert :into refs
                          :values $v1]
                         rows))))

(defun org-roam-v2-db-insert-link (link)
  "Insert link data for LINK at current point into the org-roam-v2 cache."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (let ((type (org-element-property :type link))
          (dest (org-element-property :path link))
          (properties (list :outline (org-get-outline-path)))
          (source (org-roam-v2-id-at-point)))
      (when source
        (org-roam-v2-db-query
         [:insert :into links
          :values $v1]
         (vector (point) source dest type properties))))))

;;;;; Fetching
(defun org-roam-v2-db--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let ((current-files (org-roam-v2-db-query [:select [file hash] :from files]))
        (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam-v2-db--file-hash (&optional file-path)
  "Compute the hash of FILE-PATH, a file or current buffer."
  (if file-path
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file-path)
        (secure-hash 'sha1 (current-buffer)))
    (org-with-wide-buffer
     (secure-hash 'sha1 (current-buffer)))))

;;;;; Updating
(defun org-roam-v2-db-sync (&optional force)
  "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file org-roam-v2-db-location))
  (org-roam-v2-db--close) ;; Force a reconnect
  (org-roam-v2-db) ;; To initialize the database, no-op if already initialized
  (let* ((gc-cons-threshold org-roam-v2-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-v2-files (org-roam-v2--list-all-files))
         (current-files (org-roam-v2-db--get-current-files))
         (modified-files nil))
    (dolist (file org-roam-v2-files)
      (let ((contents-hash (org-roam-v2-db--file-hash file)))
        (unless (string= (gethash file current-files)
                         contents-hash)
          (push file modified-files)))
      (remhash file current-files))
    (emacsql-with-transaction (org-roam-v2-db)
      (if (fboundp 'dolist-with-progress-reporter)
          (dolist-with-progress-reporter (file (hash-table-keys current-files))
              "Clearing removed files..."
            (org-roam-v2-db-clear-file file))
        (dolist (file (hash-table-keys current-files))
          (org-roam-v2-db-clear-file file)))
      (if (fboundp 'dolist-with-progress-reporter)
          (dolist-with-progress-reporter (file modified-files)
              "Processing modified files..."
            (org-roam-v2-db-update-file file))
        (dolist (file modified-files)
          (org-roam-v2-db-update-file file))))))

(defun org-roam-v2-db-update-file (&optional file-path)
  "Update org-roam-v2 cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-v2-db--file-hash file-path))
        (db-hash (caar (org-roam-v2-db-query [:select hash :from files
                                           :where (= file $s1)] file-path))))
    (unless (string= content-hash db-hash)
      (org-roam-v2-with-file file-path nil
        (save-excursion
          (org-set-regexps-and-options 'tags-only)
          (org-roam-v2-db-clear-file)
          (org-roam-v2-db-insert-file)
          (org-roam-v2-db-insert-file-node)
          (org-roam-v2-db-map-headlines
           (list #'org-roam-v2-db-insert-node-data
                 #'org-roam-v2-db-insert-aliases
                 #'org-roam-v2-db-insert-tags
                 #'org-roam-v2-db-insert-refs))
          (org-roam-v2-db-map-links
           (list #'org-roam-v2-db-insert-link)))))))

(defun org-roam-v2-db--update-on-save-h ()
  "."
  (add-hook 'after-save-hook #'org-roam-v2-db-update-file nil t))

(add-hook 'org-roam-v2-find-file-hook #'org-roam-v2-db--update-on-save-h)

;; Diagnostic Interactives
(defun org-roam-v2-db-diagnose-node ()
  "Print information about node at point."
  (interactive)
  (prin1 (org-roam-v2-node-at-point)))

(provide 'org-roam-v2-db)

;;; org-roam-v2-db.el ends here
