;;; lang/data/config.el -*- lexical-binding: t; -*-

(use-package json-mode
  :mode ("\\.json$" . json-mode))

;; toml
(use-package toml-mode
  :mode ("\\.toml$" . toml-mode))

;; yaml
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))
