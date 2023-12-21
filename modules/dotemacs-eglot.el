;;; dotemacs-eglot.el --- eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(consult-eglot))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                         (eglot-ensure))))
  :init
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-events-buffer-size 0))

(use-package consult-eglot
  :defer t)

(provide 'dotemacs-eglot)
;;; dotemacs-eglot.el ends here
