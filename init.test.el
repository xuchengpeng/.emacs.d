;;; init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

(dotemacs! :completion
           company
           ivy
           :ui
           (window-select +switch-window)
           workspaces
           :emacs
           dired
           electric
           keybinds
           :tools
           project
           utils
           :lang
           markdown
           sh)

;;
;;; Bootstrap dotemacs
(dotemacs-initialize)
