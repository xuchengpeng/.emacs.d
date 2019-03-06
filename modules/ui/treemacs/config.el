;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(use-package treemacs
  :commands (treemacs)
  :config
  (setq treemacs-follow-after-init     t
        treemacs-is-never-other-window t
        treemacs-sorting               'alphabetic-case-insensitive-desc
        treemacs-persist-file          (expand-file-name "treemacs-persist" dotemacs-cache-dir))
  
  ;; Don't follow the cursor
  (treemacs-follow-mode -1))

(use-package treemacs-projectile
  :after treemacs projectile)
