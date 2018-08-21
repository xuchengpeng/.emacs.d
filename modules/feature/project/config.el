;;; feature/project/config.el -*- lexical-binding: t -*-

(use-package projectile
  :commands (projectile-project-root projectile-project-name projectile-project-p)
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-dir "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile-bookmarks.eld")
        projectile-require-project-root nil)
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
