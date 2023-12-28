;;; dotemacs-org.el --- org-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'org "9.6.14")
(dotemacs-require-package 'htmlize)

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
  (defun dotemacs-org-post-file ()
    (let* ((filename (read-from-minibuffer "New post filename: "))
           (post-dir (concat dotemacs-org-site-dir "/org/posts/" (format-time-string "%Y"))))
      (unless (file-exists-p post-dir)
        (make-directory post-dir t))
      (find-file (expand-file-name filename post-dir))
      (tempel-insert 'blog-title)))
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(expand-file-name "todo.org" org-directory) "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)
          ("n" "Notes" entry
           (file+headline ,(expand-file-name "notes.org" org-directory) "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
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
        org-html-head-include-scripts nil
        org-html-validation-link nil)
  (setq org-publish-project-alist
        `(;; Publish the posts
          ("site-org"
            :base-directory ,(concat dotemacs-org-site-dir "/org")
            :publishing-directory ,(concat dotemacs-org-site-dir "/public")
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
            :html-postamble-format ,dotemacs-org-html-postamble-format
            )
          ("site-js"
            :base-directory ,(concat dotemacs-org-site-dir "/js")
            :base-extension "js"
            :publishing-directory ,(concat dotemacs-org-site-dir "/public/js")
            :recursive t
            :publishing-function org-publish-attachment
            )
          ("site-css"
            :base-directory ,(concat dotemacs-org-site-dir "/css")
            :base-extension "css"
            :publishing-directory ,(concat dotemacs-org-site-dir "/public/css")
            :recursive t
            :publishing-function org-publish-attachment
            )
          ("site-images"
            :base-directory ,(concat dotemacs-org-site-dir "/images")
            :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
            :publishing-directory ,(concat dotemacs-org-site-dir "/public/images")
            :recursive t
            :publishing-function org-publish-attachment
            )
          ("site" :components ("site-org" "site-js" "site-css" "site-images")))))

(provide 'dotemacs-org)
;;; dotemacs-org.el ends here
