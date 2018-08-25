;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           ;; benchmark
           editor
           ;; evil
           highlight
           lookup
           project
           snippets
           syntax-checker
           utils
           :completion
           company
           (helm +fuzzy)
           ;; (ivy +fuzzy)
           :ui
           modeline
           ;; modeline-old
           ;; neotree
           themes
           treemacs
           (window-select +ace-window)
           :tools
           editorconfig
           eshell
           magit
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
