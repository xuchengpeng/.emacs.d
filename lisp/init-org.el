;;; init-org.el --- org-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org$" . org-mode)
  :hook ((org-mode . (lambda () (electric-indent-local-mode -1))))
  :init
  (keymap-global-set "C-c a" 'org-agenda)
  (keymap-global-set "C-c c" 'org-capture)
  :config
  (keymap-set org-mode-map "C-c t" '("Toggle" . (keymap)))
  (keymap-set org-mode-map "C-c t i" 'org-toggle-inline-images)
  (keymap-set org-mode-map "C-c t l" 'org-toggle-link-display)
  (setq org-directory dotemacs-org-dir
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-agenda-files (list org-directory)
        org-persist-directory (expand-file-name "org-persist" dotemacs-cache-dir)
        org-publish-timestamp-directory (expand-file-name "org-timestamps/" dotemacs-cache-dir)
        org-startup-indented t)

  (defun +org-capture-org-blog-post ()
    (let* ((filename (read-from-minibuffer "New post: "))
           (post-dir (expand-file-name "org/posts" dotemacs-org-blog-dir)))
      (unless (file-exists-p post-dir)
        (make-directory post-dir t))
      (find-file (expand-file-name (format "%s-%s.org" (format-time-string "%Y%m%d") filename) post-dir))
      (insert "#+TITLE: \n" "#+DATE: " (format-time-string "<%Y-%m-%d %a %H:%M>") "\n")))

  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        `(("t" "Todo" entry
           (file ,(expand-file-name "todo.org" org-directory))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Notes" entry
           (file ,(expand-file-name "notes.org" org-directory))
           "* :NOTE: %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry
           (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
           "* %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("o" "Org Blog Post" plain
           (function +org-capture-org-blog-post)
           "" :jump-to-captured t :immediate-finish t)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "HOLD(h)" "WAIT(w)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("STARTED" . font-lock-constant-face)
          ("HOLD" . warning)
          ("WAIT" . warning)
          ("CANCELLED" . error)
          ("PROJECT" . font-lock-doc-face)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (python . t)
     (js . t)
     (css . t)
     (sass . t)
     (shell . t)
     (plantuml . t))))

(use-package org-clock
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" dotemacs-cache-dir))
  :config
  (setq org-clock-persist 'history
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-history-length 20)
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package htmlize
  :ensure t)

(use-package ox-publish
  :commands org-publish
  :config
  (setq org-export-in-background t
        org-export-headline-levels 4
        org-export-with-section-numbers nil
        org-export-with-author nil
        org-export-with-priority t
        org-export-with-toc t
        org-export-time-stamp-file nil
        org-html-container-element "section"
        org-html-divs '((preamble  "header" "preamble")
                        (content   "main" "content")
                        (postamble "footer" "postamble"))
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-validation-link nil)

  (defcustom +org-html-stable-ids t
    "Non-nil means to use stable IDs in the exported document."
    :type 'boolean)

  (defun +org-html-stable-ids-extract-id (datum)
    "Extract a reference from a DATUM.

Return DATUM's `:CUSTOM_ID` if set, or generate a reference from its
`:raw-value` property.  If the DATUM does not have either, return
nil."
    (or
     (org-element-property :CUSTOM_ID datum)
     (let ((value (org-element-property :raw-value datum)))
       (when value
         (+org-html-stable-ids-to-kebab-case value)))))

  (defun +org-html-stable-ids-to-kebab-case (string)
    "Convert STRING to kebab-case."
    (string-trim
     (replace-regexp-in-string
      "[^a-z0-9]+" "-"
      (downcase string))
     "-" "-"))

  (defun +org-export-stable-ids-get-reference (datum info)
    "Return a reference for DATUM with INFO.

    Raise an error if the ID was used in the document before."
    (let ((cache (plist-get info :internal-references))
	      (id (+org-html-stable-ids-extract-id datum)))
	  (or (car (rassq datum cache))
	      (if (assoc id cache)
		      (user-error "Duplicate ID: %s" id)
	        (when id
		      (push (cons id datum) cache)
		      (plist-put info :internal-references cache)
		      id)))))

  (defun +org-html-stable-ids-reference (datum info &optional named-only)
    "Call `org-export-get-reference` to get a reference for DATUM with INFO.

If `NAMED-ONLY` is non-nil, return nil."
    (unless named-only
      (org-export-get-reference datum info)))

  (when +org-html-stable-ids
    (advice-add #'org-export-get-reference :override #'+org-export-stable-ids-get-reference)
    (advice-add #'org-html--reference :override #'+org-html-stable-ids-reference))

  (defun +org-publish-sitemap (title list)
    (concat "#+TITLE: " title "\n"
            "#+OPTIONS: toc:nil\n\n"
            (org-list-to-org list)))

  (defun +org-publish-sitemap-format-entry (entry style project)
    (cond ((not (directory-name-p entry))
           (format "%s [[file:%s][%s]]"
                   (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
                   entry
                   (org-publish-find-title entry project)))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry)))

  (setq org-publish-project-alist
        `(("blog-posts"
           :base-directory ,(expand-file-name "org/posts" dotemacs-org-blog-dir)
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :publishing-directory ,(expand-file-name "public/posts" dotemacs-org-blog-dir)
           :description ,(format "This is %s's personal website, powered by Emacs & Org mode." user-full-name)
           :keywords ,(format "%s, blog, technology, programming" user-full-name)
           :html-head ,dotemacs-org-html-head
           :html-preamble ,dotemacs-org-html-preamble
           :html-postamble ,dotemacs-org-html-postamble
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Posts"
           :sitemap-format-entry +org-publish-sitemap-format-entry
           :sitemap-function +org-publish-sitemap
           :sitemap-sort-files anti-chronologically)
          ("blog-pages"
           :base-directory ,(expand-file-name "org" dotemacs-org-blog-dir)
           :base-extension "org"
           :recursive nil
           :publishing-function org-html-publish-to-html
           :publishing-directory ,(expand-file-name "public" dotemacs-org-blog-dir)
           :description ,(format "This is %s's personal website, powered by Emacs & Org mode." user-full-name)
           :keywords ,(format "%s, blog, technology, programming" user-full-name)
           :html-head ,dotemacs-org-html-head
           :html-preamble ,dotemacs-org-html-preamble
           :html-postamble ,dotemacs-org-html-postamble
           :auto-sitemap nil)
          ("blog-static"
           :base-directory ,(expand-file-name "org" dotemacs-org-blog-dir)
           :base-extension "js\\|css\\|jpg\\|gif\\|png\\|svg\\|gif\\|ico\\|txt\\|webmanifest"
           :recursive t
           :publishing-function org-publish-attachment
           :publishing-directory ,(expand-file-name "public" dotemacs-org-blog-dir))
          ("blog" :components ("blog-posts" "blog-pages" "blog-static")))))

(provide 'init-org)
;;; init-org.el ends here
