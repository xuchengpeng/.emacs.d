;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           keybinds
           project
           workspaces
           :completion
           company
           (ivy +fuzzy)
           :ui
           (window-select +switch-window)
           :emacs
           dired
           electric
           :tools
           utils
           :lang
           markdown)
