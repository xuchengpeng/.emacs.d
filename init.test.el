;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :completion
           company
           helm
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
