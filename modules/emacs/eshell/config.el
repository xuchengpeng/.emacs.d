;;; emacs/eshell/config.el -*- lexical-binding: t; -*-

(use-package eshell
  :commands (eshell eshell-mode)
  :config
  (setq eshell-buffer-shorthand t
        eshell-history-size 1000
        eshell-save-history-on-exit t
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-cmpl-ignore-case t
        ;; em-glob
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-directory-name (concat dotemacs-cache-dir "eshell")
        ;; em-alias
        eshell-aliases-file (concat dotemacs-local-dir ".eshell-aliases")
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-prompt)
    
    ;; Consider eshell buffers real
    (add-hook 'eshell-mode-hook #'dotemacs|mark-buffer-as-real))

(use-package shell-pop
  :commands (shell-pop)
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))
