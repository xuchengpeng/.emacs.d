;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           evil
           project
           workspaces
           :completion
           company
           (helm +fuzzy)
           :ui
           (window-select +ace-window)
           :emacs
           dired
           :tools
           utils
           :lang
           markdown)
