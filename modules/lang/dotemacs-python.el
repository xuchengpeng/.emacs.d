;;; dotemacs-python.el --- python-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(lsp-pyright))

(use-package python
  :defer t
  :init
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'dotemacs-python)
;;; dotemacs-python.el ends here
