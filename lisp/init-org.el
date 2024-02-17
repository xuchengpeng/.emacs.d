;;; init-org.el --- org-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'htmlize)

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

  (defun dotemacs-org-post-file ()
    (let* ((filename (read-from-minibuffer "New post filename: "))
           (post-dir (expand-file-name (format "org/posts/%s" (format-time-string "%Y/%m")) dotemacs-org-site-dir)))
      (unless (file-exists-p post-dir)
        (make-directory post-dir t))
      (find-file (expand-file-name filename post-dir))
      (tempel-insert 'blog-title)))
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
          ("p" "Post" plain
           (function dotemacs-org-post-file)
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
  :config
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" dotemacs-cache-dir))
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
        org-html-head-include-scripts nil
        org-html-validation-link nil)
  (setq org-publish-project-alist
        `(;; Publish the posts
          ("site-org"
           :base-directory ,(expand-file-name "org" dotemacs-org-site-dir)
           :publishing-directory ,(expand-file-name "public" dotemacs-org-site-dir)
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :auto-sitemap nil
           :section-numbers nil
           :with-author nil
           :with-priority t
           :with-toc t
           :time-stamp-file nil
           :html-doctype "html5"
           :html-html5-fancy t
           :html-head ,dotemacs-org-html-head
           :html-preamble t
           :html-preamble-format ,dotemacs-org-html-preamble-format
           :html-postamble t
           :html-postamble-format ,dotemacs-org-html-postamble-format)
          ("site-js"
           :base-directory ,(expand-file-name "js" dotemacs-org-site-dir)
           :base-extension "js"
           :publishing-directory ,(expand-file-name "public/js" dotemacs-org-site-dir)
           :recursive t
           :publishing-function org-publish-attachment)
          ("site-css"
           :base-directory ,(expand-file-name "css" dotemacs-org-site-dir)
           :base-extension "css"
           :publishing-directory ,(expand-file-name "public/css" dotemacs-org-site-dir)
           :recursive t
           :publishing-function org-publish-attachment)
          ("site-images"
           :base-directory ,(expand-file-name "images" dotemacs-org-site-dir)
           :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
           :publishing-directory ,(expand-file-name "public/images" dotemacs-org-site-dir)
           :recursive t
           :publishing-function org-publish-attachment)
          ("site" :components ("site-org" "site-js" "site-css" "site-images")))))

(provide 'init-org)
;;; init-org.el ends here
