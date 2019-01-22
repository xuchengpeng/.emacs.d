;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           ;; benchmark
           editor
           ;; evil
           highlight
           keybinds
           ;; lookup
           project
           snippets
           (syntax-checker +flymake) ; +flycheck or +flymake
           workspaces
           :completion
           company
           helm
           ;; ido
           ;; ivy
           :ui
           dashboard
           modeline
           ;; modeline-old
           ;; neotree
           themes
           treemacs
           (window-select +ace-window)
           :emacs
           dired
           ediff
           electric
           eshell
           :tools
           ;; deft
           ;; editorconfig
           ;; impatient-mode
           ;; magit
           ;; package-manager
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
           )
