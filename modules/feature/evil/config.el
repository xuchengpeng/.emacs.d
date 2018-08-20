;;; feature/evil/config.el -*- lexical-binding: t; -*-

(use-package evil
  :hook (dotemacs-post-init . evil-mode))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :hook (dotemacs-post-init . global-evil-surround-mode))
