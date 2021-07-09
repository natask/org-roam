;;; org-roam-v2-overlay.el --- Link overlay for org-roam-v2 nodes -*- coding: utf-8; lexical-binding: t; -*-

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

(defface org-roam-v2-overlay
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "black"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "white")))
  "Face for the org-roam-v2 overlay."
  :group 'org-roam-v2-faces)

(defun org-roam-v2-overlay--make (l r &rest props)
  "Make an overlay from L to R with PROPS."
  (let ((o (make-overlay l (or r l))))
    (overlay-put o 'category 'org-roam-v2)
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun org-roam-v2-overlay-make-link-overlay (link)
  "Create overlay for LINK."
  (save-excursion
    (save-match-data
      (let* ((type (org-element-property :type link))
             (id (org-element-property :path link))
             (pos (org-element-property :end link))
             (desc-p (org-element-property :contents-begin link))
             node)
        (when (and (string-equal type "id")
                   (setq node (org-roam-v2-node-from-id id))
                   (not desc-p))
          (org-roam-v2-overlay--make
           pos pos
           'after-string (format "%s "
                                 (propertize (org-roam-v2-node-title node)
                                             'face 'org-roam-v2-overlay))))))))

(defun org-roam-v2-overlay-enable ()
  "Enable org-roam-v2 overlays."
  (org-roam-v2-db-map-links
   (list #'org-roam-v2-overlay-make-link-overlay)))

(defun org-roam-v2-overlay-disable ()
  "Disable org-roam-v2 overlays."
  (remove-overlays nil nil 'category 'org-roam-v2))

(defun org-roam-v2-overlay-redisplay ()
  "Redisplay org-roam-v2 overlays."
  (org-roam-v2-overlay-disable)
  (org-roam-v2-overlay-enable))

(define-minor-mode org-roam-v2-overlay-mode
  "Overlays for org-roam-v2 ID links.
org-roam-v2 overlay mode is a minor mode.  When enabled,
overlay displaying the node's title is displayed."
  (if org-roam-v2-overlay-mode
      (progn
        (org-roam-v2-overlay-enable)
        (add-hook #'after-save-hook #'org-roam-v2-overlay-redisplay nil t))
    (org-roam-v2-overlay-disable)
    (remove-hook #'after-save-hook #'org-roam-v2-overlay-redisplay t)))

(provide 'org-roam-v2-overlay)
;;; org-roam-v2-overlay.el ends here
