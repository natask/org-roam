;;; org-roam-v2.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Dependencies
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam-v2--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 'seq)
(require 'magit-section)
(eval-when-compile (require 'subr-x))

;;;; Features
(require 'org-roam-v2-compat)
(eval-when-compile
  (require 'org-roam-v2-macs)
  (require 'org-macs))
(require 'org-roam-v2-utils)
(require 'org-roam-v2-mode)
(require 'org-roam-v2-completion)
(require 'org-roam-v2-capture)
(require 'org-roam-v2-dailies)
(require 'org-roam-v2-db)

;;; Declarations
;; From org-ref-core.el
(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
;; From org-id.el
(declare-function org-id-find-id-in-file "ext:org-id" (id file &optional markerp))

(defgroup org-roam-v2 nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-v2-"
  :link '(url-link :tag "Github" "https://github.com/org-roam-v2/org-roam-v2")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual.html"))

(defgroup org-roam-v2-faces nil
  "Faces used by org-roam-v2."
  :group 'org-roam-v2
  :group 'faces)

;;;; Variables
(defcustom org-roam-v2-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam-v2)

(defcustom org-roam-v2-directory (expand-file-name "~/org-roam-v2/")
  "Default path to org-roam-v2 files.
All Org files, at any level of nesting, are considered part of the org-roam-v2."
  :type 'directory
  :group 'org-roam-v2)

(defcustom org-roam-v2-file-extensions '("org")
  "Detected file extensions to include in the org-roam-v2 ecosystem.
The first item in the list is used as the default file extension.
While the file extensions may be different, the file format needs
to be an `org-mode' file, and it is the user's responsibility to
ensure that."
  :type '(repeat string)
  :group 'org-roam-v2)

(defcustom org-roam-v2-file-exclude-regexp nil
  "Files matching this regular expression are excluded from the org-roam-v2."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'org-roam-v2)

(defcustom org-roam-v2-list-files-commands
  (if (member system-type '(windows-nt ms-dos cygwin))
      nil
    '(find rg))
  "Commands that will be used to find org-roam-v2 files.

It should be a list of symbols or cons cells representing any of the following
 supported file search methods.

The commands will be tried in order until an executable for a command is found.
The Elisp implementation is used if no command in the list is found.

  `rg'
    Use ripgrep as the file search method.
    Example command: rg /path/to/dir/ --files -g \"*.org\" -g \"*.org.gpg\"

  `find'
    Use find as the file search method.
    Example command:
    find /path/to/dir -type f \( -name \"*.org\" -o -name \"*.org.gpg\" \)

By default, `executable-find' will be used to look up the path to the
executable. If a custom path is required, it can be specified together with the
method symbol as a cons cell. For example: '(find (rg . \"/path/to/rg\"))."
  :type '(set (const :tag "find" find)
              (const :tag "rg" rg)))

;;;; ID Utilities
(defun org-roam-v2-id-at-point ()
  "Return the ID at point, if any.
Recursively traverses up the headline tree to find the
first encapsulating ID."
  (let (source)
    (org-with-wide-buffer
     (while (and (not (setq source (org-id-get)))
                 (not (bobp)))
       (org-roam-v2-up-heading-or-point-min)))
    source))

;;;; File functions and predicates
(defun org-roam-v2--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam-v2-file-p (&optional file)
  "Return t if FILE is part of org-roam-v2 system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (let* ((path (or file (buffer-file-name (buffer-base-buffer))))
         (ext (org-roam-v2--file-name-extension path))
         (ext (if (string= ext "gpg")
                  (org-roam-v2--file-name-extension (file-name-sans-extension path))
                ext)))
    (save-match-data
      (and
       (member ext org-roam-v2-file-extensions)
       (not (and org-roam-v2-file-exclude-regexp
                 (string-match-p org-roam-v2-file-exclude-regexp path)))
       (f-descendant-of-p path (expand-file-name org-roam-v2-directory))))))

(defun org-roam-v2--shell-command-files (cmd)
  "Run CMD in the shell and return a list of files. If no files are found, an empty list is returned."
  (--> cmd
    (shell-command-to-string it)
    (ansi-color-filter-apply it)
    (split-string it "\n")
    (seq-filter #'s-present? it)))

(defun org-roam-v2--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (cl-loop for e in exts
           append (list (format "\"*.%s\"" e)
                        (format "\"*.%s.gpg\"" e))))

(defun org-roam-v2--list-files-rg (executable dir)
  "Return all org-roam-v2 files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam-v2--list-files-search-globs org-roam-v2-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam-v2--shell-command-files command)))

(defun org-roam-v2--list-files-find (executable dir)
  "Return all org-roam-v2 files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam-v2--list-files-search-globs org-roam-v2-file-extensions))
         (names (s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "-type f \\(" ,names "\\)"))))
    (org-roam-v2--shell-command-files command)))

;; Emacs 26 does not have FOLLOW-SYMLINKS in `directory-files-recursively'
(defun org-roam-v2--directory-files-recursively (dir regexp
                                                  &optional include-directories predicate
                                                  follow-symlinks)
  "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (condition-case nil
                               (org-roam-v2--directory-files-recursively
                                full-file regexp include-directories
                                predicate follow-symlinks)
                             (file-error nil))
                         (org-roam-v2--directory-files-recursively
                          full-file regexp include-directories
                          predicate follow-symlinks))))
                  (setq result (nconc result sub-files))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

(defun org-roam-v2--list-files-elisp (dir)
  "Return all org-roam-v2 files located recursively within DIR, using elisp."
  (let ((regex (concat "\\.\\(?:"(mapconcat
                                  #'regexp-quote org-roam-v2-file-extensions
                                  "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (org-roam-v2--directory-files-recursively dir regex nil nil t) result)
      (when (and (file-readable-p file)
                 (org-roam-v2-file-p file))
        (push file result)))))

(defun org-roam-v2--list-files (dir)
  "Return all org-roam-v2 files located recursively within DIR.
Use external shell commands if defined in `org-roam-v2-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-v2-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam-v2--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (seq-filter #'org-roam-v2-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam-v2--list-files-elisp dir))))

(defun org-roam-v2--list-all-files ()
  "Return a list of all org-roam-v2 files within `org-roam-v2-directory'."
  (org-roam-v2--list-files (expand-file-name org-roam-v2-directory)))

(defun org-roam-v2--nodes-table ()
  "Return a hash table of node ID to org-roam-v2-nodes."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id ,file ,title)
                   (org-roam-v2-db-query [:select [id file title] :from nodes]))
      (puthash id (org-roam-v2-node-create :file file :id id :title title) ht))
    ht))

(defun org-roam-v2--files-table ()
  "Return a hash table of file to file properties."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,file ,hash ,atime ,mtime)
                   (org-roam-v2-db-query [:select [file hash atime mtime] :from files]))
      (puthash file `(,hash ,atime ,mtime) ht))
    ht))

(defun org-roam-v2--tags-table ()
  "Return a hash table of node ID to list of tags."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,node-id ,tag) (org-roam-v2-db-query [:select [node-id tag] :from tags]))
      (puthash node-id (cons tag (gethash node-id ht)) ht))
    ht))

(defun org-roam-v2-buffer-p (&optional buffer)
  "Return t if BUFFER is accessing a part of org-roam-v2 system.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (setq path (buffer-file-name (buffer-base-buffer)))
           (org-roam-v2-file-p path)))))

(defun org-roam-v2-buffer-list ()
  "Return a list of buffers that are org-roam-v2 files."
  (--filter (org-roam-v2-buffer-p it)
            (buffer-list)))

(defun org-roam-v2--get-titles ()
  "Return all titles and aliases in the org-roam-v2 database."
  (let* ((titles (mapcar #'car (org-roam-v2-db-query [:select title :from nodes])))
         (aliases (mapcar #'car (org-roam-v2-db-query [:select alias :from aliases])))
         (completions (append titles aliases)))
    completions))

;;; org-roam-v2 setup and teardown
(defvar org-roam-v2-find-file-hook nil
  "Hook run when an org-roam-v2 file is visited.")

(defun org-roam-v2-setup ()
  "Setup org-roam-v2."
  (interactive)
  (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                   (file-executable-p emacsql-sqlite3-executable))
              (executable-find "sqlite3"))
    (lwarn '(org-roam-v2) :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'. \
M-x info for more information at org-roam-v2 > Installation > Post-Installation Tasks."))
  (add-hook 'find-file-hook #'org-roam-v2--file-setup)
  (add-hook 'kill-emacs-hook #'org-roam-v2-db--close-all)
  (advice-add 'rename-file :after #'org-roam-v2--rename-file-advice)
  (advice-add 'delete-file :before #'org-roam-v2--delete-file-advice)
  (org-roam-v2-db-sync))

(defun org-roam-v2-teardown ()
  "Teardown org-roam-v2."
  (interactive)
  (remove-hook 'find-file-hook #'org-roam-v2--file-setup)
  (remove-hook 'kill-emacs-hook #'org-roam-v2-db--close-all)
  (advice-remove 'rename-file #'org-roam-v2--rename-file-advice)
  (advice-remove 'delete-file #'org-roam-v2--delete-file-advice)
  (org-roam-v2-db--close-all)
  ;; Disable local hooks for all org-roam-v2 buffers
  (dolist (buf (org-roam-v2-buffer-list))
    (with-current-buffer buf
      (remove-hook 'after-save-hook #'org-roam-v2-db-update-file t))))

;;; Hooks and advices
(defun org-roam-v2--file-setup ()
  "Setup an org-roam-v2 file."
  (when (org-roam-v2-file-p)
    (run-hooks 'org-roam-v2-find-file-hook)))

(defun org-roam-v2--delete-file-advice (file &optional _trash)
  "Maintain cache consistency when file deletes.
FILE is removed from the database."
  (when (and (not (auto-save-file-name-p file))
             (not (backup-file-name-p file))
             (org-roam-v2-file-p file))
    (org-roam-v2-db-clear-file (expand-file-name file))))

(defun org-roam-v2--rename-file-advice (old-file new-file-or-dir &rest _args)
  "Maintain cache consistency of file rename.
OLD-FILE is cleared from the database, and NEW-FILE-OR-DIR is added."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir)))
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam-v2-file-p old-file))
      (org-roam-v2-db-clear-file old-file))
    (when (org-roam-v2-file-p new-file)
      (org-roam-v2-db-update-file new-file))))

;;;; Nodes
(cl-defstruct (org-roam-v2-node (:constructor org-roam-v2-node-create)
                             (:copier nil))
  file file-hash file-atime file-mtime
  id level point todo priority scheduled deadline title properties olp
  tags aliases refs)

(cl-defmethod org-roam-v2-node-slug ((node org-roam-v2-node))
  "Return the slug of NODE."
  (let ((title (org-roam-v2-node-title node))
        (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                      ("__*" . "_")  ;; remove sequential underscores
                      ("^_" . "")  ;; remove starting underscore
                      ("_$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defvar org-roam-v2-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-v2-mode-map)
    (define-key map [remap org-roam-v2-visit-thing] 'org-roam-v2-node-visit)
    map)
  "Keymap for org-roam-v2 node sections.")

(defclass org-roam-v2-node-section (magit-section)
  ((keymap :initform org-roam-v2-node-map)
   (node :initform nil)))

(defvar org-roam-v2-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-v2-mode-map)
    (define-key map [remap org-roam-v2-visit-thing] 'org-roam-v2-preview-visit)
    map)
  "Keymap for org-roam-v2 preview.")

(defclass org-roam-v2-preview-section (magit-section)
  ((keymap :initform org-roam-v2-preview-map)
   (file :initform nil)
   (begin :initform nil)
   (end :initform nil)))

(cl-defmethod org-roam-v2-populate ((node org-roam-v2-node))
  "Populate NODE from database.
Uses the ID, and fetches remaining details from the database.
This can be quite costly: avoid, unless dealing with very few
nodes."
  (when-let ((node-info (car (org-roam-v2-db-query [:select [file level pos todo priority
                                                          scheduled deadline title properties olp]
                                                 :from nodes
                                                 :where (= id $s1)
                                                 :limit 1]
                                                (org-roam-v2-node-id node)))))
    (let ((tag-info (mapcar #'car (org-roam-v2-db-query [:select [tag] :from tags
                                                      :where (= node-id $s1)]
                                                     (org-roam-v2-node-id node))))
          (alias-info (mapcar #'car (org-roam-v2-db-query [:select [alias] :from aliases
                                                        :where (= node-id $s1)]
                                                       (org-roam-v2-node-id node))))
          (refs-info (mapcar #'car (org-roam-v2-db-query [:select [ref] :from refs
                                                       :where (= node-id $s1)]
                                                      (org-roam-v2-node-id node)))))
      (pcase-let ((`(,file ,level ,pos ,todo ,priority ,scheduled
                           ,deadline ,title ,properties ,olp) node-info))
        (setf (org-roam-v2-node-file node) file
              (org-roam-v2-node-level node) level
              (org-roam-v2-node-point node) pos
              (org-roam-v2-node-todo node) todo
              (org-roam-v2-node-priority node) priority
              (org-roam-v2-node-scheduled node) scheduled
              (org-roam-v2-node-deadline node) deadline
              (org-roam-v2-node-title node) title
              (org-roam-v2-node-properties node) properties
              (org-roam-v2-node-olp node) olp
              (org-roam-v2-node-tags node) tag-info
              (org-roam-v2-node-refs node) refs-info
              (org-roam-v2-node-aliases node) alias-info))))
  node)

(defcustom org-roam-v2-node-display-template
  "${title:*} ${tags:10}"
  "Configures display formatting for org-roam-v2 node.
Patterns of form \"${field-name:length}\" are interpolated based
on the current node. \"field-name\" is replaced with the
corresponding value of the field of the current node. \"length\"
specifies how many characters are used to display the value of
the field. A \"length\" of \"*\" specifies that as many
characters as possible should be used."
  :group 'org-roam-v2
  :type  'string)

(defun org-roam-v2--tags-to-str (tags)
  "Convert list of TAGS into a string."
  (mapconcat (lambda (s) (concat "#" s)) tags " "))

(defun org-roam-v2-node--format-entry (node width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
Uses `org-roam-v2-node-display-template' to format the entry."
  (let ((fmt (org-roam-v2--process-display-format org-roam-v2-node-display-template)))
    (org-roam-v2-format
     (car fmt)
     (lambda (field)
       (let* ((field (split-string field ":"))
              (field-name (car field))
              (field-width (cadr field))
              (getter (intern (concat "org-roam-v2-node-" field-name)))
              (field-value (or (funcall getter node) "")))
         (when (and (equal field-name "tags")
                    field-value)
           (setq field-value (org-roam-v2--tags-to-str field-value)))
         (when (and (equal field-name "file")
                    field-value)
           (setq field-value (file-relative-name field-value org-roam-v2-directory)))
         (when (and (equal field-name "olp")
                    field-value)
           (setq field-value (string-join field-value " > ")))
         (if (not field-width)
             field-value
           (setq field-width (string-to-number field-width))
           (truncate-string-to-width
            field-value
            (if (> field-width 0)
                field-width
              (- width (cdr fmt)))
            0 ?\s)))))))

(defun org-roam-v2-node-preview (file point)
  "Get preview content for FILE at POINT."
  (save-excursion
    (org-roam-v2-with-temp-buffer file
      (goto-char point)
      (let* ((elem (org-element-at-point))
             (begin (org-element-property :begin elem))
             (end (org-element-property :end elem)))
        (list begin end
              (or (string-trim (buffer-substring-no-properties begin end))
                  (org-element-property :raw-value elem)))))))

(defun org-roam-v2-node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error if there is no node at point."
  (if-let ((node (magit-section-case
                   (org-roam-v2-node-section (oref it node))
                   (t (let (id)
                        (org-with-wide-buffer
                         (while (and (not (setq id (org-id-get)))
                                     (not (bobp)))
                           (org-roam-v2-up-heading-or-point-min))
                         (when id
                           (org-roam-v2-populate
                            (org-roam-v2-node-create :id id
                                                  :point (point))))))))))
      node
    (when assert
      (user-error "No node at point"))))

(defun org-roam-v2-node--find (node)
  "Navigate to the point for NODE, and return the buffer."
  (unless (org-roam-v2-node-file node)
    (user-error "Node does not have corresponding file"))
  (let ((buf (find-file-noselect (org-roam-v2-node-file node))))
    (with-current-buffer buf
      (goto-char (org-roam-v2-node-point node)))
    buf))

(defun org-roam-v2-node-visit (node &optional other-window)
  "From the buffer, visit NODE.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead."
  (interactive (list (org-roam-v2-node-at-point t) current-prefix-arg))
  (let ((buf (org-roam-v2-node--find node)))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defun org-roam-v2-node-from-id (id)
  "Return an `org-roam-v2-node' for the node containing ID.
Return nil if a node with ID does not exist."
  (when (> (caar (org-roam-v2-db-query [:select (funcall count) :from nodes
                                     :where (= id $s1)]
                                    id)) 0)
    (org-roam-v2-populate (org-roam-v2-node-create :id id))))

(defun org-roam-v2-node-from-title-or-alias (s)
  "Return an `org-roam-v2-node' for the node with title or alias S.
Return nil if the node does not exist.
Throw an error if multiple choices exist."
  (let ((matches (seq-uniq
                  (append
                   (org-roam-v2-db-query [:select [id] :from nodes
                                       :where (= title $s1)]
                                      s)
                   (org-roam-v2-db-query [:select [node-id] :from aliases
                                       :where (= alias $s1)]
                                      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-v2-populate (org-roam-v2-node-create :id (caar matches))))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

(defun org-roam-v2-node-list ()
  "Return a list of all nodes."
  (let ((rows (org-roam-v2-db-query
               "SELECT
  id,
  file,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-v2-node-create :id id
                                                      :file file
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

(defun org-roam-v2-node--to-candidate (node)
  "Return a minibuffer completion candidate given NODE."
  (let ((candidate-main (org-roam-v2-node--format-entry node (1- (frame-width))))
        (tag-str (org-roam-v2--tags-to-str (org-roam-v2-node-tags node))))
    (cons (propertize (concat candidate-main
                              (propertize tag-str 'invisible t))
                      'node node)
          node)))

(defun org-roam-v2-node--completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-v2-node'.
The displayed title is formatted according to `org-roam-v2-node-display-template'."
  (setq org-roam-v2--cached-display-format nil)
  (let ((nodes (org-roam-v2-node-list)))
    (mapcar #'org-roam-v2-node--to-candidate nodes)))

(defcustom org-roam-v2-node-annotation-function #'org-roam-v2-node--annotation
  "The function used to return annotations in the minibuffer for org-roam-v2 nodes.
This function takes a single argument NODE, which is an `org-roam-v2-node' construct."
  :group 'org-roam-v2
  :type 'function)

(defcustom org-roam-v2-node-default-sort #'file-mtime
  "Default sort order for org-roam-v2 node completions."
  :type 'const
  :group 'org-roam-v2)

(defun org-roam-v2-node-sort-by-file-mtime (completion-a completion-b)
  "Sort files such that files modified more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of (node-title org-roam-v2-node-struct)"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-v2-node-file-mtime node-b)
                 (org-roam-v2-node-file-mtime node-a))))

(defun org-roam-v2-node-sort-by-file-atime (completion-a completion-b)
  "Sort files such that files accessed more recently are shown first.
COMPLETION-A and COMPLETION-B are items in the form of (node-title org-roam-v2-node-struct)"
  "Sort completions list by file modification time."
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (time-less-p (org-roam-v2-node-file-atime node-b)
                 (org-roam-v2-node-file-atime node-a))))

(defun org-roam-v2-node-read (&optional initial-input filter-fn sort-fn require-match)
  "Read and return an `org-roam-v2-node'.
INITIAL-INPUT is the initial minibuffer prompt value. FILTER-FN
is a function to filter out nodes: it takes a single argument (an
`org-roam-v2-node'), and when nil is returned the node will be
filtered out.
SORT-FN is a function to sort nodes. See `org-roam-v2-node-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match."
  (let* ((nodes (org-roam-v2-node--completions))
         (nodes (cl-remove-if-not (lambda (n)
                                    (if filter-fn (funcall filter-fn (cdr n)) t)) nodes))
         (sort-fn (or sort-fn
                      (when org-roam-v2-node-default-sort
                        (intern (concat "org-roam-v2-node-sort-by-" (symbol-name org-roam-v2-node-default-sort))))))
         (_ (when sort-fn (setq nodes (seq-sort sort-fn nodes))))
         (node (completing-read
                "Node: "
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function . (lambda (title)
                                                 (funcall org-roam-v2-node-annotation-function
                                                          (get-text-property 0 'node title))))
                        (category . org-roam-v2-node))
                    (complete-with-action action nodes string pred)))
                nil require-match initial-input)))
    (or (cdr (assoc node nodes))
        (org-roam-v2-node-create :title node))))

(defun org-roam-v2-node--annotation (_node)
  "Dummy function.
Returns empty string for annotations."
  "")

(defun org-roam-v2-preview-visit (file point &optional other-window)
  "Visit FILE at POINT.
With prefix argument OTHER-WINDOW, visit the olp in another
window instead."
  (interactive (list (org-roam-v2-file-at-point t)
                     (oref (magit-current-section) begin)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char point))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(cl-defun org-roam-v2-node-insert-section (&key source-node point properties)
  "Insert section for NODE.
SOURCE-NODE is the source node.
POINT is the point in buffer for the link.
PROPERTIES contains properties about the link."
  (magit-insert-section section (org-roam-v2-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat (propertize (org-roam-v2-node-title source-node)
                                  'font-lock-face 'org-roam-v2-title)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-v2-olp)))))
    (magit-insert-heading)
    (oset section node source-node)
    (magit-insert-section section (org-roam-v2-preview-section)
      (pcase-let ((`(,begin ,end ,s) (org-roam-v2-node-preview (org-roam-v2-node-file source-node)
                                                            point)))
        (insert (org-roam-v2-fontify-like-in-org-mode s) "\n")
        (oset section file (org-roam-v2-node-file source-node))
        (oset section begin begin)
        (oset section end end))
      (insert ?\n))))

;;;###autoload
(defun org-roam-v2-node-find (&optional other-window initial-input filter-fn)
  "Find and open an org-roam-v2 node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive current-prefix-arg)
  (let ((node (org-roam-v2-node-read initial-input filter-fn)))
    (if (org-roam-v2-node-file node)
        (org-roam-v2-node-visit node other-window)
      (org-roam-v2-capture-
       :node node
       :props '(:finalize find-file)))))

(defun org-roam-v2-node-insert (&optional filter-fn)
  "Find an org-roam-v2 file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-v2-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-v2-node-title node))))
          (if (org-roam-v2-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-v2-node-id node))
                         description)))
            (org-roam-v2-capture-
             :node node
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-v2-node-random (&optional other-window)
  "Find a random org-roam-v2 node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead."
  (interactive current-prefix-arg)
  (let ((random-row (seq-random-elt (org-roam-v2-db-query [:select [id file pos] :from nodes]))))
    (org-roam-v2-node-visit (org-roam-v2-node-create :id (nth 0 random-row)
                                               :file (nth 1 random-row)
                                               :point (nth 2 random-row))
                         other-window)))

;;;; Properties
(defun org-roam-v2-add-property (s prop)
  "Add S to property PROP."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (lst (if (memq s lst) lst (cons s lst))))
    (org-set-property prop (combine-and-quote-strings lst))
    s))

(defun org-roam-v2-remove-property (prop &optional s)
  "Remove S from property PROP.

If S is not specified, user is prompted to select a value."
  (let* ((p (org-entry-get (point) prop))
         (lst (when p (split-string-and-unquote p)))
         (prop-to-remove (or s (completing-read "Remove: " lst)))
         (lst (delete prop-to-remove lst)))
    (if lst
        (org-set-property prop (combine-and-quote-strings lst))
      (org-delete-property prop))
    prop-to-remove))

(defun org-roam-v2-set-keyword (key value)
  "Set keyword KEY to VALUE.
If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") (point-max) t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) 'fixedcase nil nil 1))
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" key ": " value "\n")))))

;;;; Tags
(defun org-roam-v2-tag-completions ()
  "Return list of tags for completions within org-roam-v2."
  (let ((roam-tags (mapcar #'car (org-roam-v2-db-query [:select :distinct [tag] :from tags])))
        (org-tags (cl-loop for tagg in org-tag-alist
                           nconc (pcase tagg
                                   ('(:newline)
                                    nil)
                                   (`(,tag . ,_)
                                    (list tag))
                                   (_ nil)))))
    (seq-uniq (append roam-tags org-tags))))

(defun org-roam-v2-tag-add (tags)
  "Add TAGS to the node at point."
  (interactive
   (list (completing-read-multiple "Tag: " (org-roam-v2-tag-completions))))
  (let ((node (org-roam-v2-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-v2-node-point node))
      (if (= (org-outline-level) 0)
          (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                             (org-collect-keywords '("filetags"))))
                                                ""))))
            (org-roam-v2-set-keyword "filetags" (string-join (seq-uniq (append tags current-tags)) " ")))
        (org-set-tags (seq-uniq (append tags (org-get-tags)))))
      tags)))

(defun org-roam-v2-tag-remove (&optional tags)
  "Remove TAGS from the node at point."
  (interactive)
  (let ((node (org-roam-v2-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-v2-node-point node))
      (if (= (org-outline-level) 0)
          (let* ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                              (org-collect-keywords '("filetags"))))
                                                 (user-error "No tag to remove"))))
                 (tags (or tags (completing-read-multiple "Tag: " current-tags))))
            (org-roam-v2-set-keyword "filetags"
                                  (string-join (seq-difference current-tags tags #'string-equal) " ")))
        (let* ((current-tags (or (org-get-tags)
                                 (user-error "No tag to remove")))
               (tags (completing-read-multiple "Tag: " current-tags)))
          (org-set-tags (seq-difference current-tags tags #'string-equal))))
      tags)))

;;;; Aliases
(defun org-roam-v2-alias-add (alias)
  "Add ALIAS to the node at point."
  (interactive "sAlias: ")
  (let ((node (org-roam-v2-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-v2-node-point node))
      (org-roam-v2-add-property alias "ROAM_ALIASES"))))

(defun org-roam-v2-alias-remove (&optional alias)
  "Remove an ALIAS from the node at point."
  (interactive)
  (let ((node (org-roam-v2-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-v2-node-point node))
      (org-roam-v2-remove-property "ROAM_ALIASES" alias))))

;;;; Refs
(defun org-roam-v2-ref-add (ref)
  "Add REF to the node at point."
  (interactive "sRef: ")
  (let ((node (org-roam-v2-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-v2-node-point node))
      (org-roam-v2-add-property ref "ROAM_REFS"))))

(defun org-roam-v2-ref-remove (&optional ref)
  "Remove a REF from the node at point."
  (interactive)
  (let ((node (org-roam-v2-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-v2-node-point node))
      (org-roam-v2-remove-property "ROAM_REFS" ref))))

(defun org-roam-v2-ref--completions ()
  "Return an alist for ref completion.
The car is the ref, and the cdr is the corresponding node for the ref."
  nil
  (let ((rows (org-roam-v2-db-query
               [:select [id ref type nodes:file pos title]
                :from refs
                :left-join nodes
                :on (= refs:node-id nodes:id)])))
    (cl-loop for row in rows
             collect (pcase-let* ((`(,id ,ref ,type ,file ,pos ,title) row)
                                  (node (org-roam-v2-node-create :id id
                                                              :file file
                                                              :point pos
                                                              :title title)))
                       (cons (propertize ref 'node node 'type type)
                             node)))))

(defun org-roam-v2-ref-read (&optional initial-input filter-fn)
  "Read an org-roam-v2 ref.
Return a string, is propertized in `meta' with additional properties.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function applied to the completion list."
  (let* ((refs (org-roam-v2-ref--completions))
         (refs (funcall (or filter-fn #'identity) refs))
         (ref (completing-read "Ref: "
                               (lambda (string pred action)
                                 (if (eq action 'metadata)
                                     '(metadata
                                       (annotation-function . org-roam-v2-ref--annotation)
                                       (category . org-roam-v2-ref))
                                   (complete-with-action action refs string pred)))
                               nil t initial-input)))
    (cdr (assoc ref refs))))

(defun org-roam-v2-ref--annotation (ref)
  "Return the annotation for REF.
REF is assumed to be a propertized string."
  (let* ((node (get-text-property 0 'node ref))
         (title (org-roam-v2-node-title node)))
    (when title
      (concat " " title))))

(defun org-roam-v2-ref-find (&optional initial-input filter-fn)
  "Find and open and org-roam-v2 file from REF if it exists.
REF should be the value of '#+roam_key:' without any
type-information (e.g. 'cite:').
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is applied to the ref list to filter out candidates."
  (interactive)
  (let* ((node (org-roam-v2-ref-read initial-input filter-fn)))
    (find-file (org-roam-v2-node-file node))
    (goto-char (org-roam-v2-node-point node))))

;;;; roam: link
(defcustom org-roam-v2-link-auto-replace t
  "When non-nil, replace org-roam-v2's roam links with file or id links whenever possible."
  :group 'org-roam-v2
  :type 'boolean)

;;; the roam: link
(org-link-set-parameters "roam" :follow #'org-roam-v2-link-follow-link)

(defun org-roam-v2-link-replace-at-point (&optional link)
  "Replace the roam: LINK at point with an id link."
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             node)
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "roam")
                   (setq node (org-roam-v2-node-from-title-or-alias path)))
          (replace-match (org-link-make-string
                          (concat "id:" (org-roam-v2-node-id node))
                          path)))))))

(defun org-roam-v2-link-replace-all ()
  "Replace all \"roam:\" links in buffer with \"id:\" links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (org-roam-v2-link-replace-at-point))))

(defun org-roam-v2--replace-roam-links-on-save-h ()
  "Run `org-roam-v2-link-replace-all' before buffer is saved to its file."
  (when org-roam-v2-link-auto-replace
    (add-hook 'before-save-hook #'org-roam-v2-link-replace-all nil t)))

(add-hook 'org-roam-v2-find-file-hook #'org-roam-v2--replace-roam-links-on-save-h)

(defun org-roam-v2-link-follow-link (path)
  "org-roam-v2's roam: link navigation with description PATH.
This function is called by Org when following links of the type
`roam'. While the path is passed, assume that the cursor is on
the link."
  (if-let ((node (org-roam-v2-node-from-title-or-alias path)))
      (progn
        (when org-roam-v2-link-auto-replace
          (org-roam-v2-link-replace-at-point))
        (org-id-goto (org-roam-v2-node-id node)))
    (org-roam-v2-capture-
     :node (org-roam-v2-node-create :title path)
     :props '(:finalize find-file))))

(defun org-roam-v2-open-id-at-point ()
  "Navigates to the ID at point.
To be added to `org-open-at-point-functions'."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (id (org-element-property :path context)))
    (when (string= type "id")
      (let ((node (org-roam-v2-populate (org-roam-v2-node-create :id id))))
        (cond
         ((org-roam-v2-node-file node)
          (org-mark-ring-push)
          (org-roam-v2-node-visit node)
          t)
         (t nil))))))

(defun org-roam-v2-open-id-with-org-roam-v2-db-h ()
  "."
  (add-hook 'org-open-at-point-functions #'org-roam-v2-open-id-at-point nil t))

(add-hook 'org-roam-v2-find-file-hook #'org-roam-v2-open-id-with-org-roam-v2-db-h)

;;; Refiling
(defun org-roam-v2-demote-entire-buffer ()
  "Convert an org buffer with any top level content to a single node.

All headings are demoted one level.

The #+TITLE: keyword is converted into a level-1 heading and deleted.
Any tags declared on #+FILETAGS: are transferred to tags on the new top heading.

Any top level properties drawers are incorporated into the new heading."
  (interactive)
  (org-with-point-at 1
    (org-map-entries 'org-do-demote)
    (insert "* "
            (org-roam-v2--file-keyword-get "TITLE")
            "\n")
    (org-back-to-heading)
    (org-set-tags (org-roam-v2--file-keyword-get "FILETAGS"))
    (org-roam-v2--file-keyword-kill "TITLE")
    (org-roam-v2--file-keyword-kill "FILETAGS")))

(defun org-roam-v2-refile ()
  "Refile to node."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-v2-node-read nil nil nil 'require-match))
         (file (org-roam-v2-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if regionp
        (progn
          (org-kill-new (buffer-substring region-start region-end))
          (org-save-markers-in-region region-start region-end))
      (progn
        (if (org-before-first-heading-p)
            (org-roam-v2-demote-entire-buffer))
        (org-copy-subtree 1 nil t)))
    (with-current-buffer nbuf
      (org-with-wide-buffer
       (goto-char (org-roam-v2-node-point node))
       (setq level (org-get-valid-level (funcall outline-level) 1)
             reversed (org-notes-order-reversed-p))
       (goto-char
        (if reversed
            (or (outline-next-heading) (point-max))
          (or (save-excursion (org-get-next-sibling))
              (org-end-of-subtree t t)
              (point-max))))
       (unless (bolp) (newline))
       (org-paste-subtree level nil nil t)
       (and org-auto-align-tags
            (let ((org-loop-over-headlines-in-active-region nil))
              (org-align-tags)))
       (when (fboundp 'deactivate-mark) (deactivate-mark))))
    (if regionp
        (delete-region (point) (+ (point) (- region-end region-start)))
      (org-preserve-local-variables
       (delete-region
        (and (org-back-to-heading t) (point))
        (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
    (org-roam-v2--kill-empty-buffer)))

(provide 'org-roam-v2)
;;; org-roam-v2.el ends here
