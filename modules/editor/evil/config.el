;;; editor/evil/config.el -*- lexical-binding: t; -*-

(use-package evil
  :hook (after-init . evil-mode))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))
