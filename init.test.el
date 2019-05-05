;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           keybinds
           project
           :completion
           company
           ivy
           :ui
           (window-select +switch-window)
           workspaces
           :emacs
           dired
           electric
           :tools
           utils
           :lang
           markdown)
