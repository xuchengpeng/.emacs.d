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
    (let* ((filename (read-from-minibuffer "New post filename: "))
           (post-dir (expand-file-name (format "org/posts/%s" (format-time-string "%Y/%m")) dotemacs-org-blog-dir)))
      (unless (file-exists-p post-dir)
        (make-directory post-dir t))
      (find-file (expand-file-name filename post-dir))
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
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-validation-link nil)
  (setq org-publish-project-alist
        `(("blog-posts"
           :base-directory ,(expand-file-name "org" dotemacs-org-blog-dir)
           :publishing-directory ,(expand-file-name "public" dotemacs-org-blog-dir)
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :description ,(format "This is %s's personal website, published with Emacs Org mode." user-full-name)
           :keywords ,(format "%s, blog, technology, programming" user-full-name)
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
          ("blog-static"
           :base-directory ,(expand-file-name "org" dotemacs-org-blog-dir)
           :base-extension "js\\|css\\|jpg\\|gif\\|png\\|svg\\|gif\\|ico\\|webmanifest"
           :publishing-directory ,(expand-file-name "public" dotemacs-org-blog-dir)
           :recursive t
           :publishing-function org-publish-attachment)
          ("blog" :components ("blog-posts" "blog-static")))))

(provide 'init-org)
;;; init-org.el ends here
