;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines 0))
(add-to-list 'default-frame-alist '(menu-bar-lines 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
