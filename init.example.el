;;; init.el -*- lexical-binding: t; -*-

(unless (boundp 'dotemacs)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

(dotemacs! :completion
           company
           ;; helm
           ;; ido
           ;; ivy
           vertico
           
           :editor
           ;; evil
           snippets
           utils
           
           :ui
           dashboard
           highlight
           modeline
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
           ;; go
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

;;
;;; Bootstrap dotemacs
(dotemacs-initialize)
