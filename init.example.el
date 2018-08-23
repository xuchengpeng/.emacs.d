;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           ;; benchmark
           ;; evil
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
           :ui
           modeline
           ;; modeline-old
           treemacs
           ;; neotree
           (window-select +ace-window)
           :tools
           eshell
           magit
           editorconfig
           ;; package-manager
           :lang
           cc
           data
           javascript
           markdown
           (org         ; organize your plain life in plain text
            +capture    ; org-capture in and outside of Emacs
            +export)    ; Exporting org to whatever you want
           web)
