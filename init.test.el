;;; init.el -*- lexical-binding: t; -*-

(unless (boundp 'dotemacs)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

(dotemacs! :completion
           company
           vertico
           :ui
           (window-select +switch-window)
           workspaces
           :emacs
           dired
           electric
           keybinds
           :tools
           lsp
           project
           utils
           :lang
           markdown
           sh)

;;
;;; Bootstrap dotemacs
(dotemacs-initialize)
