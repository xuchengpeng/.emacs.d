;;; lang/data/config.el -*- lexical-binding: t; -*-

(use-package json-mode
  :mode ("\\.json$" . json-mode)
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?})))

;; toml
(use-package toml-mode
  :mode ("\\.toml$" . toml-mode))

;; yaml
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))
