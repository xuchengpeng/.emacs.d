;;; tools/project/config.el -*- lexical-binding: t -*-

(defvar dotemacs-projectile-fd-binary
  (or (cl-find-if #'executable-find '("fdfind" "fd"))
      "fd")
  "name of `fd-find' executable binary")

(use-package projectile
  :commands (projectile-project-root projectile-project-name projectile-project-p)
  :init
  (define-key! projectile-mode-map
    "C-c p"           'projectile-command-map)
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-dir "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-dir "projectile.projects")
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-files-cache-expire 604800 ; expire after a week
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

  (projectile-mode +1)
  
  (cond
   ;; If fd exists, use it for git and generic projects. fd is a rust program
   ;; that is significantly faster than git ls-files or find, and it respects
   ;; .gitignore. This is recommended in the projectile docs.
   ((executable-find dotemacs-projectile-fd-binary)
    (setq projectile-generic-command
          (format "%s . --color=never --type f -0 -H -E .git"
                  dotemacs-projectile-fd-binary)
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get fd's benefits
          projectile-indexing-method 'alien))

   ;; Otherwise, resort to ripgrep, which is also faster than find
   ((executable-find "rg")
    (setq projectile-generic-command
          (concat "rg -0 --files --color=never --hidden"
                  (cl-loop for dir in projectile-globally-ignored-directories
                           concat (format " --glob '!%s'" dir)))
          projectile-git-command projectile-generic-command
          projectile-git-submodule-command nil
          ;; ensure Windows users get rg's benefits
          projectile-indexing-method 'alien))

   ;; Fix breakage on windows in git projects with submodules, since Windows
   ;; doesn't have tr
   ((not (executable-find "tr"))
    (setq projectile-git-submodule-command nil))))
