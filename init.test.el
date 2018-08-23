;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           editor
           project
           :completion
           company
           ivy
           :ui
           (window-select +ace-window)
           :tools
           eshell
           :lang
           markdown
           web)
