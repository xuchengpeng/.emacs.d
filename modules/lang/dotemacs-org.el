;;; dotemacs-org.el --- org-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'org "9.6.14")
(dotemacs-require-packages '(htmlize))

(defcustom dotemacs-org-dir "~/org/"
  "Org directory."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-org-site-dir nil
  "Org site directory."
  :type 'string
  :group 'dotemacs)

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory dotemacs-org-dir
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-agenda-files (list org-directory)
        org-persist-directory (concat dotemacs-cache-dir "org-persist/")
        org-publish-timestamp-directory (concat dotemacs-cache-dir "org-timestamps/"))
  :config
  (defvar +org-capture-todo-file (expand-file-name "todo.org" org-directory))
  (defvar +org-capture-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-default-notes-file +org-capture-notes-file
        org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t :kill-buffer t)
          ("n" "Notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "HOLD(h)" "WAIT(w)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("STARTED" . font-lock-constant-face)
          ("HOLD" . warning)
          ("WAIT" . warning)
          ("CANCELLED" . error)
          ("PROJECT" . font-lock-doc-face))))

(use-package org-clock
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (concat dotemacs-cache-dir "org-clock-save.el"))
  :config
  (setq org-clock-persist 'history
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-history-length 20)
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package ox-publish
  :commands org-publish-project
  :config
  (setq org-export-in-background t
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)
  (setq org-publish-project-alist
        '(;; Publish the posts
          ("site-org"
            :base-directory (concat dotemacs-org-site-dir "/org")
            :publishing-directory (concat dotemacs-org-site-dir "/public")
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
            :base-directory (concat dotemacs-org-site-dir "/js")
            :base-extension "js"
            :publishing-directory (concat dotemacs-org-site-dir "/public/js")
            :recursive t
            :publishing-function org-publish-attachment
            )
          ("site-css"
            :base-directory (concat dotemacs-org-site-dir "/css")
            :base-extension "css"
            :publishing-directory (concat dotemacs-org-site-dir "/public/css")
            :recursive t
            :publishing-function org-publish-attachment
            )
          ("site-images"
            :base-directory (concat dotemacs-org-site-dir "/images")
            :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
            :publishing-directory (concat dotemacs-org-site-dir "/public/images")
            :recursive t
            :publishing-function org-publish-attachment
            )
          ("site" :components ("site-org" "site-js" "site-css" "site-images")))))

(provide 'dotemacs-org)
;;; dotemacs-org.el ends here
