;; init-org.el --- Initialize org configurations.
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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
;; Org configurations.
;;

;;; Code:

(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.txt$" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (defvar dotemacs-org-directory (concat dotemacs-dir "org/"))
  (setq org-directory dotemacs-org-directory
        org-agenda-files (list dotemacs-org-directory))
  (setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE")))
  (setq org-log-done 'time
        org-startup-indented t
        org-startup-folded nil
        org-clock-persist t
        org-clock-persist-file (concat dotemacs-cache-directory "org-clock-save.el")
        org-clock-in-resume t
        org-clock-into-drawer t
        org-log-into-drawer t)
  (org-clock-persistence-insinuate)
  
  (dotemacs-after-load 'org-capture
    (defun dotemacs-org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
       See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title))
             (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time))))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ,(concat ":EXPORT_DATE: " date)
                     ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :comments true :mathjax false"
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))
    
    (add-to-list 'org-capture-templates
                 '("h"                ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Hugo Posts" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file+olp "hugo-posts.org" "Hugo Posts")
                   (function dotemacs-org-hugo-new-subtree-post-capture-template)))
    
    (add-to-list 'org-capture-templates '("t" "Tasks"))
    (add-to-list 'org-capture-templates
                 '("tt" "Task" entry
                   (file+headline "task.org" "Task")
                   "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))
    (add-to-list 'org-capture-templates
                 '("tw" "Work Task" entry
                   (file+headline "work.org" "Work")
                   "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))
    )
  )

(use-package ox-hugo
  :disabled
  :after ox)

(provide 'init-org)

;;; init-org.el ends here
