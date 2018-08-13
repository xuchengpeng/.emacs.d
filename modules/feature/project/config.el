;;; feature/project/config.el -*- lexical-binding: t -*-

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-dir "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile-bookmarks.eld"))
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  :config
  (projectile-mode))
