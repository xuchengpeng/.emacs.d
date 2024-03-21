;;; init-shell.el -- Init shell. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if IS-WINDOWS
    (require 'init-eshell)
  (dotemacs-require-package 'vterm)
  (use-package vterm
    :commands (vterm vterm-other-window)
    :config
    (add-hook
     'vterm-mode-hook
     (lambda ()
       (setq-local dotemacs-modeline-left '(dotemacs-modeline--window-number
                                            dotemacs-modeline--buffer-default-directory)
                   dotemacs-modeline-right '(dotemacs-modeline--major-mode))))))

(defun dotemacs-shell ()
  "Launch shell."
  (interactive)
  (if IS-WINDOWS
      (eshell)
    (vterm)))

(provide 'init-shell)
;;; init-shell.el ends here
