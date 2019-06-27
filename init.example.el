;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :completion
           company
           helm
           ;; ido
           ;; ivy
           
           :editor
           ;; evil
           snippets
           utils
           
           :ui
           dashboard
           highlight
           modeline
           ;; neotree
           themes
           treemacs
           (window-select +ace-window)
           workspaces
           
           :emacs
           dired
           ediff
           electric
           eshell
           keybinds
           
           :tools
           ;; benchmark
           ;; deft
           ;; editorconfig
           flycheck
           ;; impatient-mode
           ;; lookup
           ;; magit
           ;; package-manager
           project
           utils
           
           :lang
           cc
           ;; data
           go
           ;; javascript
           markdown
           (org         ; organize your plain life in plain text
            +capture    ; org-capture in and outside of Emacs
            +export     ; Exporting org to whatever you want
            +publish    ; Publish org to html blog
            )
           ;; plantuml
           )
