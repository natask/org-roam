;;; org-roam-v2-mode.el --- create and refresh Org-roam buffers -*- lexical-binding: t -*-
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
;; This library implements the abstract major-mode `org-roam-v2-mode', from which
;; almost all other org-roam-v2 major-modes derive.
;;
;;; Code:
(require 'magit-section)

(require 'org-roam-v2-utils)

(defvar org-roam-v2-directory)
(defvar org-roam-v2-find-file-hook)

(declare-function org-roam-v2-node-at-point "org-roam-v2")

;;; Faces
(defface org-roam-v2-header-line
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for the `header-line' in some org-roam-v2 modes."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-title
  '((t :weight bold))
  "Face for org-roam-v2 titles."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-olp
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the OLP of the node."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-preview-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey25"
     :foreground "grey70"))
  "Face for preview headings."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-preview-heading-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey35"
     :foreground "grey70"))
  "Face for current preview headings."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-preview-heading-selection
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit org-roam-v2-preview-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit org-roam-v2-preview-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected preview headings."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-preview-region
  `((t :inherit bold
       ,@(and (>= emacs-major-version 27)
              (list :extend (ignore-errors (face-attribute 'region :extend))))))
  "Face used by `org-roam-v2-highlight-preview-region-using-face'.

This face is overlaid over text that uses other hunk faces,
and those normally set the foreground and background colors.
The `:foreground' and especially the `:background' properties
should be avoided here.  Setting the latter would cause the
loss of information.  Good properties to set here are `:weight'
and `:slant'."
  :group 'org-roam-v2-faces)

(defface org-roam-v2-dim
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the dimmer part of the widgets."
  :group 'org-roam-v2-faces)

;;; Variables
(defvar org-roam-v2-current-node nil
  "The current node at point.")

(defvar org-roam-v2-current-directory nil
  "The `org-roam-v2-directory' value for the current node.")

(defcustom org-roam-v2-mode-section-functions (list #'org-roam-v2-backlinks-section
                                                 #'org-roam-v2-reflinks-section)
  "Functions which insert sections of the `org-roam-v2-buffer'.
Each function is called with one argument, which is the current org-roam-v2 node at point."
  :group 'org-roam-v2
  :type 'hook)

;;; The mode
(defvar org-roam-v2-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'org-roam-v2-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-v2-visit-thing)
    (define-key map [remap revert-buffer] 'org-roam-v2-buffer-render)
    map)
  "Parent keymap for all keymaps of modes derived from `org-roam-v2-mode'.")

(define-derived-mode org-roam-v2-mode magit-section-mode "org-roam-v2"
  "Major mode for org-roam-v2's buffer."
  :group 'org-roam-v2
  (face-remap-add-relative 'header-line 'org-roam-v2-header-line))

;;; Key functions
(defun org-roam-v2-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

(defun org-roam-v2-buffer-render ()
  "Render the current node at point."
  (interactive)
  (when (derived-mode-p 'org-roam-v2-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq-local default-directory org-roam-v2-current-directory)
      (setq-local org-roam-v2-directory org-roam-v2-current-directory)
      (org-roam-v2-set-header-line-format (org-roam-v2-node-title org-roam-v2-current-node))
      (magit-insert-section (org-roam-v2)
        (magit-insert-heading)
        (run-hook-with-args 'org-roam-v2-mode-section-functions org-roam-v2-current-node)))))

(defun org-roam-v2-buffer ()
  "Launch an org-roam-v2 buffer for the current node at point."
  (interactive)
  (if-let ((node (org-roam-v2-node-at-point))
           (source-org-roam-v2-directory org-roam-v2-directory))
      (progn
        (let ((buffer (get-buffer-create
                       (concat "org-roam-v2: "
                               (file-relative-name (buffer-file-name) org-roam-v2-directory)))))
          (with-current-buffer buffer
            (org-roam-v2-mode)
            (setq-local org-roam-v2-current-node node)
            (setq-local org-roam-v2-current-directory source-org-roam-v2-directory)
            (org-roam-v2-buffer-render))
          (switch-to-buffer-other-window buffer)))
    (user-error "No node at point")))

;;; Persistent buffer
(defvar org-roam-v2-buffer "*org-roam-v2*"
  "The persistent org-roam-v2 buffer name.")

(defun org-roam-v2-buffer--post-command-h ()
  "Reconstructs the org-roam-v2 buffer.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
the org-roam-v2 buffer."
  (when (get-buffer-window org-roam-v2-buffer)
    (when-let ((node (org-roam-v2-node-at-point)))
      (unless (equal node org-roam-v2-current-node)
        (setq org-roam-v2-current-node node)
        (setq org-roam-v2-current-directory org-roam-v2-directory)
        (org-roam-v2-buffer-persistent-redisplay)))))

(define-inline org-roam-v2-buffer--visibility ()
  "Return whether the current visibility state of the org-roam-v2 buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-v2-buffer) 'visible)
    ((get-buffer org-roam-v2-buffer) 'exists)
    (t 'none))))

(defun org-roam-v2-buffer-toggle ()
  "Toggle display of the org-roam-v2 buffer."
  (interactive)
  (pcase (org-roam-v2-buffer--visibility)
    ('visible
     (progn
       (delete-window (get-buffer-window org-roam-v2-buffer))
       (remove-hook 'post-command-hook #'org-roam-v2-buffer--post-command-h)))
    ((or 'exists 'none)
     (progn
       (setq org-roam-v2-current-node (org-roam-v2-node-at-point)
             org-roam-v2-current-directory org-roam-v2-directory)
       (display-buffer (get-buffer-create org-roam-v2-buffer))
       (org-roam-v2-buffer-persistent-redisplay)))))

(defun org-roam-v2-buffer-persistent-redisplay ()
  "Recompute contents of the persistent org-roam-v2 buffer.
Has no effect when `org-roam-v2-current-node' is nil."
  (when org-roam-v2-current-node
    (with-current-buffer (get-buffer-create org-roam-v2-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-v2-mode)
        (setq-local default-directory org-roam-v2-current-directory)
        (setq-local org-roam-v2-directory org-roam-v2-current-directory)
        (org-roam-v2-set-header-line-format (org-roam-v2-node-title org-roam-v2-current-node))
        (magit-insert-section (org-roam-v2)
          (magit-insert-heading)
          (dolist (fn org-roam-v2-mode-section-functions)
            (funcall fn org-roam-v2-current-node)))))))

(defun org-roam-v2-buffer--redisplay ()
  "."
  (add-hook 'post-command-hook #'org-roam-v2-buffer--post-command-h nil t))

(add-hook 'org-roam-v2-find-file-hook #'org-roam-v2-buffer--redisplay)

;;; Sections
;;;; Backlinks
(cl-defstruct (org-roam-v2-backlink (:constructor org-roam-v2-backlink-create)
                                 (:copier nil))
  source-node target-node
  point properties)

(cl-defmethod org-roam-v2-populate ((backlink org-roam-v2-backlink))
  "Populate BACKLINK from database."
  (setf (org-roam-v2-backlink-source-node backlink)
        (org-roam-v2-populate (org-roam-v2-backlink-source-node backlink))
        (org-roam-v2-backlink-target-node backlink)
        (org-roam-v2-populate (org-roam-v2-backlink-target-node backlink)))
  backlink)

(defun org-roam-v2-backlinks-get (node)
  "Return the backlinks for NODE."
  (let ((backlinks (org-roam-v2-db-query
                    [:select [source dest pos properties]
                     :from links
                     :where (= dest $s1)
                     :and (= type "id")]
                    (org-roam-v2-node-id node))))
    (cl-loop for backlink in backlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                       (org-roam-v2-populate
                        (org-roam-v2-backlink-create
                         :source-node (org-roam-v2-node-create :id source-id)
                         :target-node (org-roam-v2-node-create :id dest-id)
                         :point pos
                         :properties properties))))))

(defun org-roam-v2-backlinks-sort (a b)
  "Default sorting function for backlinks A and B.
Sorts by title."
  (string< (org-roam-v2-node-title (org-roam-v2-backlink-source-node a))
           (org-roam-v2-node-title (org-roam-v2-backlink-source-node b))))

(defun org-roam-v2-backlinks-section (node)
  "The backlinks section for NODE."
  (when-let ((backlinks (seq-sort #'org-roam-v2-backlinks-sort (org-roam-v2-backlinks-get node))))
    (magit-insert-section (org-roam-v2-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (backlink backlinks)
        (org-roam-v2-node-insert-section
         :source-node (org-roam-v2-backlink-source-node backlink)
         :point (org-roam-v2-backlink-point backlink)
         :properties (org-roam-v2-backlink-properties backlink)))
      (insert ?\n))))

;;;; Reflinks
(cl-defstruct (org-roam-v2-reflink (:constructor org-roam-v2-reflink-create)
                                (:copier nil))
  source-node ref
  point properties)

(cl-defmethod org-roam-v2-populate ((reflink org-roam-v2-reflink))
  "Populate REFLINK from database."
  (setf (org-roam-v2-reflink-source-node reflink)
        (org-roam-v2-populate (org-roam-v2-reflink-source-node reflink)))
  reflink)

(defun org-roam-v2-reflinks-get (node)
  "Return the reflinks for NODE."
  (let ((refs (org-roam-v2-db-query [:select [ref] :from refs
                                  :where (= node-id $s1)]
                                 (org-roam-v2-node-id node)))
        links)
    (pcase-dolist (`(,ref) refs)
      (pcase-dolist (`(,source-id ,pos ,properties) (org-roam-v2-db-query
                                                     [:select [source pos properties]
                                                      :from links
                                                      :where (= dest $s1)]
                                                     ref))
        (push (org-roam-v2-populate
               (org-roam-v2-reflink-create
                :source-node (org-roam-v2-node-create :id source-id)
                :ref ref
                :point pos
                :properties properties)) links)))
    links))

(defun org-roam-v2-reflinks-sort (a b)
  "Default sorting function for reflinks A and B.
Sorts by title."
  (string< (org-roam-v2-node-title (org-roam-v2-reflink-source-node a))
           (org-roam-v2-node-title (org-roam-v2-reflink-source-node b))))

(defun org-roam-v2-reflinks-section (node)
  "The reflinks section for NODE."
  (when (org-roam-v2-node-refs node)
    (let* ((reflinks (seq-sort #'org-roam-v2-reflinks-sort (org-roam-v2-reflinks-get node))))
      (magit-insert-section (org-roam-v2-reflinks)
        (magit-insert-heading "Reflinks:")
        (dolist (reflink reflinks)
          (org-roam-v2-node-insert-section
           :source-node (org-roam-v2-reflink-source-node reflink)
           :point (org-roam-v2-reflink-point reflink)
           :properties (org-roam-v2-reflink-properties reflink)))
        (insert ?\n)))))

;;;; Unlinked references
(defvar org-roam-v2-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-v2-mode-map)
    (define-key map [remap org-roam-v2-visit-thing] 'org-roam-v2-file-visit)
    map)
  "Keymap for org-roam-v2 grep result sections.")

(defclass org-roam-v2-grep-section (magit-section)
  ((keymap :initform org-roam-v2-grep-map)
   (file :initform nil)
   (row :initform nil)
   (col :initform nil)))

(defun org-roam-v2-file-at-point (&optional assert)
  "Return the file at point.
If ASSERT, throw an error."
  (if-let ((file (magit-section-case
                   (org-roam-v2-node-section (org-roam-v2-node-file (oref it node)))
                   (org-roam-v2-grep-section (oref it file))
                   (org-roam-v2-preview-section (oref it file)))))
      file
    (when assert
      (user-error "No file at point"))))

(defun org-roam-v2-file-visit (file &optional other-window row col)
  "Visits FILE.
With a prefix argument OTHER-WINDOW, display the buffer in
another window instead.
If ROW, move to the row, and if COL move to the COL."
  (interactive (list (org-roam-v2-file-at-point t)
                     current-prefix-arg
                     (oref (magit-current-section) row)
                     (oref (magit-current-section) col)))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (when row
        (forward-line (1- row)))
      (when col
        (forward-char (1- col))))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defvar org-roam-v2-unlinked-references-result-re
  (rx (group (one-or-more anything))
      ":"
      (group (one-or-more digit))
      ":"
      (group (one-or-more digit))
      ":"
      (group (zero-or-more anything)))
  "Regex for the return result of a ripgrep query.")

(defun org-roam-v2-unlinked-references-preview-line (file row)
  "Return the preview line from FILE.
This is the ROW within FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (forward-line (1- row))
    (buffer-substring-no-properties
     (save-excursion
       (beginning-of-line)
       (point))
     (save-excursion
       (end-of-line)
       (point)))))

(defun org-roam-v2-unlinked-references-section (node)
  "The unlinked references section for NODE.
References from FILE are excluded."
  (when (and (executable-find "rg")
             (not (string-match "PCRE2 is not available"
                                (shell-command-to-string "rg --pcre2-version"))))
    (let* ((titles (cons (org-roam-v2-node-title node)
                         (org-roam-v2-node-aliases node)))
           (rg-command (concat "rg -o --vimgrep -P -i "
                               (mapconcat (lambda (glob) (concat "-g " glob))
                                          (org-roam-v2--list-files-search-globs org-roam-v2-file-extensions)
                                          " ")
                               (format " '\\[([^[]]++|(?R))*\\]%s' "
                                       (mapconcat (lambda (title)
                                                    (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                  titles ""))
                               org-roam-v2-directory))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           f row col match)
      (magit-insert-section (unlinked-references)
        (magit-insert-heading "Unlinked References:")
        (dolist (line results)
          (save-match-data
            (when (string-match org-roam-v2-unlinked-references-result-re line)
              (setq f (match-string 1 line)
                    row (string-to-number (match-string 2 line))
                    col (string-to-number (match-string 3 line))
                    match (match-string 4 line))
              (when (and match
                         (not (f-equal-p (org-roam-v2-node-file node) f))
                         (member (downcase match) (mapcar #'downcase titles)))
                (magit-insert-section section (org-roam-v2-grep-section)
                  (oset section file f)
                  (oset section row row)
                  (oset section col col)
                  (insert (propertize (format "%s:%s:%s"
                                              (truncate-string-to-width (file-name-base f) 15 nil nil "...")
                                              row col) 'font-lock-face 'org-roam-v2-dim)
                          " "
                          (org-roam-v2-fontify-like-in-org-mode
                           (org-roam-v2-unlinked-references-preview-line f row))
                          "\n"))))))
        (insert ?\n)))))

(provide 'org-roam-v2-mode)
;;; org-roam-v2-mode.el ends here
