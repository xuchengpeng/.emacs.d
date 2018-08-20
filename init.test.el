;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :ui
           (window-select +ace-window)
           :feature
           evil
           editor
           project
           utils
           :completion
           ;; helm
           ivy
           company
           :tools
           eshell
           :lang
           markdown
           web
           )
