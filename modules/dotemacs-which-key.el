;;; dotemacs-vertico.el --- emacs-which-key. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'which-key)

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-add-column-padding 1))

(provide 'dotemacs-which-key)
;;; dotemacs-which-key.el ends here