;;; feature/evil/config.el -*- lexical-binding: t; -*-

(use-package evil
  :defer 1
  :config
  (evil-mode +1))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))
