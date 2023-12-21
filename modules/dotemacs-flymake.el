;;; dotemacs-flymake.el --- flymake. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(provide 'dotemacs-flymake)
;;; dotemacs-flymake.el ends here
