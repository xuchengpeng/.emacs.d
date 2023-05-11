;;; dotemacs-flycheck.el --- Flycheck.

(dotemacs-require-packages '(flycheck))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (prog-mode . global-flycheck-mode)
  :config
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0
        flycheck-display-errors-delay 0.25))

(provide 'dotemacs-flycheck)
;;; dotemacs-flycheck.el ends here