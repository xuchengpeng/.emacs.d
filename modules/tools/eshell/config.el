;;; tools/eshell/config.el -*- lexical-binding: t; -*-

(use-package eshell
  :commands (eshell)
  :config
  (setq
       eshell-buffer-shorthand t
       eshell-history-size 1000
       eshell-save-history-on-exit t
       eshell-hist-ignoredups t
       eshell-cmpl-ignore-case t
       eshell-error-if-no-glob t
       eshell-glob-case-insensitive t
       eshell-scroll-to-bottom-on-input 'all
       eshell-directory-name (concat dotemacs-cache-dir "eshell")
       eshell-aliases-file (concat dotemacs-local-dir "eshell/alias")))

(use-package shell-pop
  :commands (shell-pop)
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))
