;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat (dir!) "/dotemacs-modeline"))
(require 'dotemacs-modeline)
(add-hook 'after-init-hook #'dotemacs-modeline-mode)
