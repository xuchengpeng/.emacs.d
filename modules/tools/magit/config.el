;;; tools/magit/config.el -*- lexical-binding: t; -*-

(use-package magit
  :commands magit-file-delete
  :config
  (setq magit-auto-revert-mode nil))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))
