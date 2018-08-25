;;; early-init.el -*- lexical-binding: t; -*-

(unless noninteractive
  (defvar file-name-handler-alist-old file-name-handler-alist)
  (unless after-init-time
    (setq garbage-collection-messages t)
    (setq file-name-handler-alist nil
          gc-cons-threshold 402653184
          gc-cons-percentage 1.0))
  
  (defun dotemacs|finalize ()
    "dotemacs finalize function."
    (setq file-name-handler-alist file-name-handler-alist-old
          gc-cons-threshold 16777216
          gc-cons-percentage 0.15))
  
  (add-hook 'emacs-startup-hook #'dotemacs|finalize))

(setq user-emacs-directory (file-name-directory load-file-name)
      package-enable-at-startup nil)

(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
