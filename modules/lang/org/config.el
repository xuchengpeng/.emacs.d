;;; lang/org/config.el -*- lexical-binding: t; -*-

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
        org-clock-persist-file (concat dotemacs-cache-dir "org-clock-save.el")
        org-clock-in-resume t
        org-clock-into-drawer t
        org-log-into-drawer t)
  (org-clock-persistence-insinuate)
  
  (after-load! 'org-capture
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
                   "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))))

(use-package ox-pandoc
  :after ox
  :config
  (setq org-export-directory (concat org-directory ".export/")
        org-export-backends '(ascii html latex md))
  
  (when (executable-find "pandoc")
    (add-to-list 'org-export-backends 'pandoc nil #'eq))
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)))
  
  ;; Export to a central location by default or if target isn't in
  ;; `org-directory'.
  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `org-directory'."
    (when (and (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p buffer-file-name org-directory))
      (cl-destructuring-bind (extension &optional subtreep _pubdir) args
        (let ((dir org-export-directory))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (setq args (list extension subtreep dir)))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))

(use-package ox-hugo
  :after ox)
