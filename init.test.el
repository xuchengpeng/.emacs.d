;;; init.el -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :ui
           themes
           modeline
           :feature
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
           cc
           markdown
           )

(dotemacs-finalize)
