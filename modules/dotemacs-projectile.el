;;; dotemacs-projectile.el --- Project management.

;;; Commentary:
;;
;; Projectile configuration.
;;

;;; Code:

(dotemacs-require-packages '(projectile))

(use-package projectile
  :defer 1
  :init
  (setq projectile-cache-file (concat dotemacs-cache-dir "projectile.cache")
        projectile-auto-discover nil
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile.projects"))
  :config
  (projectile-mode +1)
  (setq projectile-generic-command
        (cond
         ((executable-find "fd")
          "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git")
         ((executable-find "rg")
          "rg -0 --files --follow --color=never --hidden -g!.git")
         ("find . -type f -print0"))
        projectile-git-submodule-command nil
        projectile-indexing-method 'hybrid))

(provide 'dotemacs-projectile)
;;; dotemacs-projectile.el ends here