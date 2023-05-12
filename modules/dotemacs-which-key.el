;;; dotemacs-vertico.el --- emacs-which-key

(dotemacs-require-packages '(which-key))

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
  (which-key-mode +1))

(provide 'dotemacs-which-key)
;;; dotemacs-which-key.el ends here