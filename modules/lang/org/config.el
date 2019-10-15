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
  (defvar +org-capture-posts-file "hugo-posts.org")
  (defvar +org-capture-todo-file "todo.org")
  (defvar +org-capture-notes-file "notes.org")
  (setq org-capture-templates
        '(("h" "Hugo post" entry
           ;; It is assumed that below file is present in `org-directory'
           ;; and that it has a "Hugo Posts" heading. It can even be a
           ;; symlink pointing to the actual location of all-posts.org!
           (file+olp +org-capture-posts-file "Hugo Posts")
           (function +org-hugo-new-subtree-post-capture-template))
          ("t" "TODO")
          ("tt" "Todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %^{Task name}\n%u\n%a\n" :clock-in t :clock-resume t)
          ("tn" "Notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* TODO %^{Task name}\n%u\n%a\n" :clock-in t :clock-resume t))))

(defun +org-init-export-h ()
  (when (featurep! +export)
    (when (executable-find "pandoc")
      (use-package ox-pandoc
        :after ox
        :config
        (setq org-export-directory (concat org-directory ".export/")
              org-export-backends '(ascii html latex md))
        
        (add-to-list 'org-export-backends 'pandoc nil #'eq)
        (setq org-pandoc-options
              '((standalone . t)
                (mathjax . t)))
        
        ;; Export to a central location by default or if target isn't in
        ;; `org-directory'.
        (defun +org-export-output-file-name (args)
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
        (advice-add #'org-export-output-file-name :filter-args #'+org-export-output-file-name)))
    
    (use-package ox-hugo
      :after ox)))

(defun +org-init-publish-h ()
  (when (featurep! +publish)
    (use-package ox-publish
      :commands (org-publish-project)
      :config
      (setq org-html-validation-link nil
            org-publish-timestamp-directory (concat dotemacs-cache-dir "org-timestamps/"))
      
      (setq org-publish-project-alist
            '(;; Publish the posts
              ("blog-notes"
               :base-directory "~/org/blog"
               :base-extension "org"
               :publishing-directory "~/org/public"
               :recursive t
               :publishing-function org-html-publish-to-html
               :headline-levels 4
               :section-numbers nil
               :with-author nil
               :with-creator nil
               :with-email nil
               :with-timestamps nil
               :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\"/>"
               :html-head-include-default-style nil
               :html-head-include-scripts nil
               ; :html-preamble nil
               ; :html-postamble nil
               :html-link-home "index.html"
               :html-link-up "sitemap.html"
               :htmlized-source t
               :auto-sitemap t
               :sitemap-filename "sitemap.org"
               :sitemap-title "Sitemap"
               )
      
              ;; For static files that should remain untouched
              ("blog-static"
               :base-directory "~/org/blog"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
               :publishing-directory "~/org/public"
               :recursive t
               :publishing-function org-publish-attachment
               )
      
              ;; Combine the two previous components in a single one
              ("blog" :components ("blog-notes" "blog-static")))))))

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory "~/org/"
        org-agenda-files (list org-directory))
  (add-hook! 'org-load-hook
             #'+org-init-capture-defaults-h
             #'+org-init-export-h
             #'+org-init-publish-h)
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
  (org-clock-persistence-insinuate))
