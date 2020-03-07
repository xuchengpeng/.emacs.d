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
  (defvar +org-capture-posts-file
          (expand-file-name "hugo-posts.org" org-directory))
  (defvar +org-capture-todo-file
          (expand-file-name "todo.org" org-directory))
  (defvar +org-capture-notes-file
          (expand-file-name "notes.org" org-directory))
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
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("tn" "Notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t))))

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
  (use-package ox-publish
      :commands (org-publish-project)
      :config
      (setq org-export-in-background t
            org-publish-timestamp-directory (concat dotemacs-cache-dir "org-timestamps/")
            ;; Hide html built-in style and script.
            org-html-htmlize-output-type 'inline-css
            org-html-head-include-default-style nil
            org-html-head-include-scripts nil)
      (setq org-publish-project-alist
            '(;; Publish the posts
              ("site-org"
               :base-directory "~/site/org"
               :publishing-directory "~/site/public"
               ;; :preparation-function
               ;; :complete-function

               ;; ; Selecting files
               :base-extension "org"
               ;; :exclude "PrivatePage.org"     ;; regexp
               ;; :include
               :recursive t

               ;; ; Publishing action
               :publishing-function org-html-publish-to-html
               ;; :htmlized-source

               ;; ; Generic properties
               ;; :archived-trees	org-export-with-archived-trees
               ;; :exclude-tags	org-export-exclude-tags
               :headline-levels 4 ;; org-export-headline-levels
               ;; :language	org-export-default-language
               ;; :preserve-breaks	org-export-preserve-breaks
               :section-numbers nil	;; org-export-with-section-numbers
               ;; :select-tags	org-export-select-tags
               :with-author "xuchengpeng" ;; org-export-with-author
               ;; :with-broken-links	org-export-with-broken-links
               ;; :with-clocks	t ;; org-export-with-clocks
               ;; :with-creator nil ;; org-export-with-creator
               ;; :with-date org-export-with-date
               ;; :with-drawers	org-export-with-drawers
               ;; :with-email	org-export-with-email
               ;; :with-emphasize	org-export-with-emphasize
               ;; :with-fixed-width org-export-with-fixed-width
               ;; :with-footnotes	org-export-with-footnotes
               ;; :with-latex	org-export-with-latex
               ;; :with-planning	org-export-with-planning
               :with-priority t ;; org-export-with-priority ;
               ;; :with-properties	org-export-with-properties
               ;; :with-special-strings	org-export-with-special-strings
               ;; :with-sub-superscript	org-export-with-sub-superscripts
               ;; :with-tables	org-export-with-tables
               ;; :with-tags	org-export-with-tags
               ;; :with-tasks	org-export-with-tasks
               ;; :with-timestamps	org-export-with-timestamps
               ;; :with-title	org-export-with-title
               :with-toc t ;; org-export-with-toc
               ;; :with-todo-keywords	org-export-with-todo-keywords

               ;; ; HTML specific properties
               ;; :html-allow-name-attribute-in-anchors	org-html-allow-name-attribute-in-anchors
               ;; :html-checkbox-type	org-html-checkbox-type
               ;; :html-container	org-html-container-element
               ;; :html-divs	org-html-divs
               :html-doctype "html5" ;; org-html-doctype
               ;; :html-extension	org-html-extension
               ;; :html-footnote-format nil ;; org-html-footnote-format
               ;; :html-footnote-separator	org-html-footnote-separator
               ;; :html-footnotes-section	org-html-footnotes-section
               ;; :html-format-drawer-function	org-html-format-drawer-function
               ;; :html-format-headline-function	org-html-format-headline-function
               ;; :html-format-inlinetask-function	org-html-format-inlinetask-function
               ;; :html-head-extra	org-html-head-extra
               ;; :html-head-include-default-style	org-html-head-include-default-style
               ;; :html-head-include-scripts	org-html-head-include-scripts
               ;; :html-head	org-html-head
               ;; :html-home/up-format	org-html-home/up-format
               ;; :html-html5-fancy	org-html-html5-fancy
               ;; :html-indent	org-html-indent
               ;; :html-infojs-options	org-html-infojs-options
               ;; :html-infojs-template	org-html-infojs-template
               ;; :html-inline-image-rules	org-html-inline-image-rules
               ;; :html-inline-images	org-html-inline-images
               ;; :html-link-home	org-html-link-home
               ;; :html-link-org-files-as-html	org-html-link-org-files-as-html
               ;; :html-link-up	org-html-link-up
               ;; :html-link-use-abs-url	org-html-link-use-abs-url
               ;; :html-mathjax-options	org-html-mathjax-options
               ;; :html-mathjax-template	org-html-mathjax-template
               ;; :html-metadata-timestamp-format	org-html-metadata-timestamp-format
               ;; :html-postamble-format t ;; org-html-postamble-format
               ;; :html-postamble t ;; org-html-postamble
               ;; :html-preamble-format	org-html-preamble-format
               ;; :html-preamble nil ;; org-html-preamble
               ;; :html-self-link-headlines	org-html-self-link-headlines
               ;; :html-table-align-individual-field	de{org-html-table-align-individual-fields
               ;; :html-table-attributes	org-html-table-default-attributes
               ;; :html-table-caption-above	org-html-table-caption-above
               ;; :html-table-data-tags	org-html-table-data-tags
               ;; :html-table-header-tags	org-html-table-header-tags
               ;; :html-table-row-tags	org-html-table-row-tags
               ;; :html-table-use-header-tags-for-first-column	org-html-table-use-header-tags-for-first-column
               ;; :html-tag-class-prefix	org-html-tag-class-prefix
               ;; :html-text-markup-alist	org-html-text-markup-alist
               ;; :html-todo-kwd-class-prefix	org-html-todo-kwd-class-prefix
               ;; :html-toplevel-hlevel	org-html-toplevel-hlevel
               ;; :html-use-infojs	org-html-use-infojs
               ;; :html-validation-link	org-html-validation-link
               ;; :html-viewport	org-html-viewport
               ;; :html-wrap-src-lines	org-html-wrap-src-lines
               ;; :html-xml-declaration	org-html-xml-declaration

               ;; ; Markdown specific properties
               ;; :md-footnote-format	org-md-footnote-format
               ;; :md-footnotes-section	org-md-footnotes-section
               ;; :md-headline-style	org-md-headline-style

               ;; ; Other options
               :table-of-contents t
               ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
               )
              ("site-js"
               :base-directory "~/site/js/"
               :base-extension "js"
               :publishing-directory "~/site/public/js/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("site-css"
               :base-directory "~/site/css/"
               :base-extension "css"
               :publishing-directory "~/site/public/css/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("site-images"
               :base-directory "~/site/images/"
               :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
               :publishing-directory "~/site/public/images/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("site" :components ("site-org" "site-js" "site-css" "site-images"))))))

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
