;;; dotemacs-projectile.el --- Project management. -*- lexical-binding: t; -*-

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
        projectile-enable-caching t
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile.projects"))
  :config
  (setq projectile-generic-command
        (cond
         ((executable-find "fd")
          (concat "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git"
                  (if IS-WINDOWS " --path-separator=/")))
         ((executable-find "rg")
          (concat "rg -0 --files --follow --color=never --hidden -g!.git"
                  (if IS-WINDOWS " --path-separator=/")))
         ("find . -type f -print0"))
        projectile-git-submodule-command nil
        projectile-indexing-method 'hybrid)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(provide 'dotemacs-projectile)
;;; dotemacs-projectile.el ends here