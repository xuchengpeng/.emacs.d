;;; feature/project/config.el

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-dir "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile-bookmarks.eld"))
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode))
