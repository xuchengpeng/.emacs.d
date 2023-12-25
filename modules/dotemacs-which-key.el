;;; dotemacs-vertico.el --- emacs-which-key. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'which-key)

(use-package which-key
  :defer 1
  :hook (pre-command . which-key-mode)
  :init
  (setq which-key-idle-delay 0.5
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-setup-side-window-bottom)
  (add-hook 'which-key-init-buffer-hook (lambda () (setq line-spacing 3)))
  (which-key-mode +1))

(provide 'dotemacs-which-key)
;;; dotemacs-which-key.el ends here