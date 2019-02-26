;;; tools/flycheck/config.el -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :commands (flycheck-list-errors flycheck-buffer)
  :config
  (setq flycheck-check-syntax-automatically (delq 'new-line flycheck-check-syntax-automatically))
  (global-flycheck-mode +1))
