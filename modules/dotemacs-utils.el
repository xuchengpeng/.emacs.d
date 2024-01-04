;;; dotemacs-utils.el --- utils. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(which-key ace-window))

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-add-column-padding 1))

(use-package ace-window
  :defer t
  :hook (dotemacs-modeline-mode . (lambda () (require 'ace-window)))
  :init
  (global-set-key [remap other-window] 'ace-window))

(provide 'dotemacs-utils)
;;; dotemacs-utils.el ends here
