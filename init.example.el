;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

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
           snippets
           :completion
           ;; helm
           ivy
           company
           :tools
           eshell
           magit
           :lang
           cc
           data
           javascript
           markdown
           org
           web
           )
