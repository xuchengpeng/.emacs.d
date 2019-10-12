;;; lang/org/config.el -*- lexical-binding: t; -*-

(defun +org-hugo-new-subtree-post-capture-template ()
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

(defun +org-init-capture-defaults-h ()
  (setq org-capture-templates
        '(("h" "Hugo post" entry
           ;; It is assumed that below file is present in `org-directory'
           ;; and that it has a "Hugo Posts" heading. It can even be a
           ;; symlink pointing to the actual location of all-posts.org!
           (file+olp "hugo-posts.org" "Hugo Posts")
           (function +org-hugo-new-subtree-post-capture-template))
          ("t" "Tasks")
          ("tt" "Task" entry
           (file+headline "task.org" "Task")
           "* TODO %^{Task name}\n%u\n%a\n" :prepend t :clock-in t :clock-resume t)
          ("tw" "Work Task" entry
           (file+headline "work.org" "Work")
           "* TODO %^{Task name}\n%u\n%a\n" :clock-in t :clock-resume t))))

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory "~/org/"
        org-agenda-files (list org-directory))
  (add-hook! 'org-load-hook #'+org-init-capture-defaults-h)
  :config
  ;; System locale to use for formatting time values.
  ;; Make sure that the weekdays in the time stamps of your Org mode files and in the agenda appear in English.
  (setq system-time-locale "C")
  (setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE")))
  (setq org-log-done 'time
        org-startup-indented t
        org-startup-folded t
        org-clock-persist t
        org-clock-persist-file (concat dotemacs-cache-dir "org-clock-save.el")
        org-clock-in-resume t
        org-clock-into-drawer t
        org-log-into-drawer t)
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook #'auto-fill-mode))

(when (featurep! +export)
  (load! "+export"))
(when (featurep! +publish)
  (load! "+publish"))
