;;; init.el -*- lexical-binding: t; -*-

(load (concat (expand-file-name user-emacs-directory) "core/core"))

(dotemacs! :ui
           themes
           modeline
           window-select
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

(dotemacs-initialize-modules)
