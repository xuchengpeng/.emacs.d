;;; init.el -*- lexical-binding: t; -*-

(load (concat (expand-file-name user-emacs-directory) "core/core"))

(dotemacs! :ui
           themes
           modeline
           treemacs
           window-select
           :feature
           ;; benchmark
           editor
           project
           syntax-checker
           highlight
           utils
           :completion
           ;; helm
           ivy
           company
           :tools
           eshell
           magit
           snippets
           :lang
           cc
           data
           javascript
           markdown
           org
           web
           )
