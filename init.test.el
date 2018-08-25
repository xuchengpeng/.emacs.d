;;; init.el -*- lexical-binding: t; -*-

(unless (bound-and-true-p early-init-file)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

(require 'core (concat user-emacs-directory "core/core"))

(dotemacs! :feature
           editor
           project
           :completion
           company
           (helm +fuzzy)
           :ui
           (window-select +ace-window)
           :tools
           eshell
           :lang
           markdown
           web)
