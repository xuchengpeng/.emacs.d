;;; feature/project/config.el -*- lexical-binding: t -*-

(use-package projectile
  :commands (projectile-project-root projectile-project-name projectile-project-p)
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-dir "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile.projects")
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp"))
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

  (projectile-mode +1))
