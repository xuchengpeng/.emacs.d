;;; tools/magit/config.el -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :config
  (setq magit-auto-revert-mode nil))

(use-package magithub
  :after magit
  :preface
  (setq magithub-dir (concat dotemacs-cache-dir "magithub/"))
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/"))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))
