;;; feature/evil/config.el -*- lexical-binding: t; -*-

(use-package evil
  :config
  (add-hook 'dotemacs-post-init-hook #'evil-mode))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))
