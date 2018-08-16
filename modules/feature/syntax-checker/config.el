;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))
