;;; init-org.el --- org-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (keymap-global-set "C-c a" 'org-agenda)
  (keymap-global-set "C-c c" 'org-capture)
  :config
  (setq org-directory dotemacs-org-dir
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-agenda-files (list org-directory)
        org-agenda-window-setup 'current-window
        org-persist-directory (expand-file-name "org-persist" dotemacs-cache-dir)
        org-publish-timestamp-directory (expand-file-name "org-timestamps/" dotemacs-cache-dir)
        org-startup-indented t
        org-tags-column 0)

  (add-hook
   'org-agenda-mode-hook
   (lambda ()
     (setq-local +modeline-left '(+modeline--buffer-name)
                 +modeline-right '(+modeline--major-mode))))

  (defun +org-capture-org-blog ()
    (let* ((dir (completing-read "Select subdirectory: " '("posts" "notes")))
           (title (read-from-minibuffer "New file TITLE: "))
           (filename (downcase (string-trim (replace-regexp-in-string "[^A-Za-z0-9]+" "-" title) "-" "-"))))
      (expand-file-name
       (format "org/%s/%s-%s.org" dir (format-time-string "%Y-%m-%d") filename)
       dotemacs-org-blog-dir)))

  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        `(("t" "Todo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %?\n%i\n%a")
          ("n" "Notes" entry
           (file+headline "notes.org" "Notes")
           "* %u %?\n%i\n%a")
          ("j" "Journal" entry
           (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
           "* %U %?\n%i\n%a")
          ("o" "Org Blog" plain
           (file +org-capture-org-blog)
           "#+TITLE: \n#+AUTHOR: \n#+DESCRIPTION: \n#+KEYWORDS: \n#+DATE: %T\n" :jump-to-captured t)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "HOLD(h)" "WAIT(w)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("STARTED" . font-lock-constant-face)
          ("HOLD" . warning)
          ("WAIT" . warning)
          ("CANCELLED" . error)
          ("PROJECT" . font-lock-doc-face))
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)))

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
        org-export-use-babel nil
        org-html-checkbox-type 'html
        org-html-container-element "section"
        org-html-divs '((preamble  "header" "preamble")
                        (content   "main" "content")
                        (postamble "footer" "postamble"))
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-validation-link nil)

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
    (downcase
     (string-trim
      (replace-regexp-in-string "[^A-Za-z0-9\u4e00-\u9fa5]+" "-" string)
      "-" "-")))

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

  (defun +org-publish (oldfun project &optional force async)
    (advice-add #'org-export-get-reference :override #'+org-export-stable-ids-get-reference)
    (advice-add #'org-html--reference :override #'+org-html-stable-ids-reference)
    (funcall oldfun project force async)
    (advice-remove #'org-export-get-reference #'+org-export-stable-ids-get-reference)
    (advice-remove #'org-html--reference #'+org-html-stable-ids-reference))

  (advice-add #'org-publish :around #'+org-publish)

  (defun +org-publish-sitemap (title list)
    (concat "#+TITLE: " title "\n"
            "#+OPTIONS: toc:nil\n\n"
            (org-list-to-org list)))

  (defun +org-publish-sitemap-format-entry (entry style project)
    (cond ((not (directory-name-p entry))
           (format "[[file:%s][%s - %s]]"
                   entry
                   (format-time-string "%b %d, %Y" (org-publish-find-date entry project))
                   (org-publish-find-title entry project)))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry)))

  (defun +org-blog-file-contents (file)
    (with-temp-buffer
      (insert-file-contents (expand-file-name file dotemacs-org-blog-dir))
      (buffer-string)))

  (defcustom +org-html-head (+org-blog-file-contents "html/head.html")
    "Html head."
    :type 'string)

  (defcustom +org-html-header (+org-blog-file-contents "html/header.html")
    "Html header."
    :type 'string)

  (defcustom +org-html-footer (+org-blog-file-contents "html/footer.html")
    "Html footer."
    :type 'string)

  (setq org-publish-project-alist
        `(("blog-posts"
           :base-directory ,(expand-file-name "org/posts" dotemacs-org-blog-dir)
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :publishing-directory ,(expand-file-name "public/posts" dotemacs-org-blog-dir)
           :html-head ,+org-html-head
           :html-preamble ,+org-html-header
           :html-postamble ,+org-html-footer
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Posts"
           :sitemap-format-entry +org-publish-sitemap-format-entry
           :sitemap-function +org-publish-sitemap
           :sitemap-sort-files anti-chronologically)
          ("blog-notes"
           :base-directory ,(expand-file-name "org/notes" dotemacs-org-blog-dir)
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :publishing-directory ,(expand-file-name "public/notes" dotemacs-org-blog-dir)
           :html-head ,+org-html-head
           :html-preamble ,+org-html-header
           :html-postamble ,+org-html-footer
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Notes"
           :sitemap-format-entry +org-publish-sitemap-format-entry
           :sitemap-function +org-publish-sitemap
           :sitemap-sort-files anti-chronologically)
          ("blog-pages"
           :base-directory ,(expand-file-name "org" dotemacs-org-blog-dir)
           :base-extension "org"
           :recursive nil
           :publishing-function org-html-publish-to-html
           :publishing-directory ,(expand-file-name "public" dotemacs-org-blog-dir)
           :html-head ,+org-html-head
           :html-preamble ,+org-html-header
           :html-postamble ,+org-html-footer
           :auto-sitemap nil)
          ("blog-static"
           :base-directory ,(expand-file-name "org" dotemacs-org-blog-dir)
           :base-extension "js\\|css\\|jpg\\|png\\|svg\\|gif\\|ico\\|txt\\|webmanifest"
           :recursive t
           :publishing-function org-publish-attachment
           :publishing-directory ,(expand-file-name "public" dotemacs-org-blog-dir))
          ("blog" :components ("blog-posts" "blog-notes" "blog-pages" "blog-static")))))

(provide 'init-org)
;;; init-org.el ends here
