;;; init.el -*- lexical-binding: t; -*-

(unless (bound-and-true-p early-init-file)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

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
           workspaces
           :completion
           company
           (helm +fuzzy)
           ;; ido
           ;; (ivy +fuzzy)
           :ui
           modeline
           ;; modeline-old
           ;; neotree
           themes
           treemacs
           (window-select +ace-window)
           :emacs
           eshell
           :tools
           editorconfig
           ;; impatient-mode
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
