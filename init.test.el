;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :ui
           (window-select +ace-window)
           :feature
           editor
           project
           :completion
           ivy
           company
           :tools
           eshell
           :lang
           markdown
           web)
