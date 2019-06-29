;;; lang/go/config.el -*- lexical-binding: t; -*-

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(when (featurep! :completion company)
  (use-package company-go
    :after go-mode
    :config
    (set-company-backend! 'go-mode 'company-go)
    (setq company-go-show-annotation t)))
