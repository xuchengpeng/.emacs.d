;;; dotemacs-flycheck.el --- Flycheck.

;;; Commentary:
;;
;; Flycheck configuration.
;;

;;; Code:

(dotemacs-require-packages '(flycheck))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (prog-mode . global-flycheck-mode)
  :config
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0
        flycheck-display-errors-delay 0.25)
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

(provide 'dotemacs-flycheck)
;;; dotemacs-flycheck.el ends here