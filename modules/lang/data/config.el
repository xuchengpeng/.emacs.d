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

(use-package csv-mode
  :mode (("\\.csv$" . csv-mode)
         ("\\.tsv$" . tsv-mode))
  :config
  (map-local! csv-mode-map
    "a" 'csv-align-fields
    "u" 'csv-unalign-fields
    "s" 'csv-sort-fields
    "S" 'csv-sort-numeric-fields
    "k" 'csv-kill-fields
    "t" 'csv-transpose))
