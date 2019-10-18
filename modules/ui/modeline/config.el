;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(use-package dotemacs-modeline
  :straight (dotemacs-modeline :type git :host github :repo "xuchengpeng/dotemacs-modeline")
  :hook (after-init . dotemacs-modeline-mode))
