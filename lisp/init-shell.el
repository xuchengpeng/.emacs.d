;;; init-shell.el -- Init shell. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if IS-WINDOWS
    (require 'init-eshell)
  (dotemacs-require-package 'vterm)
  (use-package vterm
    :commands (vterm vterm-other-window)))

(defun dotemacs-shell ()
  "Launch shell."
  (interactive)
  (if IS-WINDOWS
      (eshell)
    (vterm)))

(provide 'init-shell)
;;; init-shell.el ends here
