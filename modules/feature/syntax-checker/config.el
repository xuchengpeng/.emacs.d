;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :commands (flycheck-list-errors flycheck-buffer)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))
