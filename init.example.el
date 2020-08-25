;;; init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

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
            )
           ;; plantuml
           ;; python
           sh
           ;; web
           )
