;;; dotemacs-tempel.el --- TempEl. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'tempel)

(use-package tempel
  :commands (tempel-complete tempel-insert)
  :init
  (global-set-key (kbd "M-+") 'tempel-complete)
  (global-set-key (kbd "M-*") 'tempel-insert)

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (setq tempel-path (nconc (ensure-list tempel-path) dotemacs-tempel-path)))

(provide 'dotemacs-tempel)
;;; dotemacs-tempel.el ends here
