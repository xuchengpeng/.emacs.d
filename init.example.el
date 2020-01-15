;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :completion
           company
           ;; helm
           ;; ido
           ivy
           
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
           electric
           eshell
           keybinds
           
           :tools
           ;; benchmark
           ;; deft
           ;; editorconfig
           flycheck
           ;; impatient-mode
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
            ;; +export     ; Exporting org to whatever you want
            +publish    ; Publish org to html blog
            )
           ;; plantuml
           ;; python
           sh
           ;; web
           )
