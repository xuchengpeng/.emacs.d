;;; lang/cc/config.el -*- lexical-binding: t; -*-

;; c
(use-package cc-mode
  :commands (c-mode c++-mode objc-mode)
  :hook((c-mode . lsp-deferred)
        (c++-mode . lsp-deferred))
  :init
  (setq-default c-basic-offset tab-width
                c-default-style "linux")
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
  :config
  (set-electric! '(c-mode c++-mode objc-mode) :chars '(?\n ?\} ?\{))
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode))
