;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           project
           workspaces
           :completion
           company
           (helm +fuzzy)
           :ui
           (window-select +ace-window)
           :emacs
           dired
           :lang
           markdown
           web)
