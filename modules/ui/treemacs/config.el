;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(use-package treemacs
  :commands (treemacs)
  :bind ("<f8>" . treemacs)
  :config
  (setq treemacs-deferred-git-apply-delay   0.5
        treemacs-display-in-side-window     t
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name "treemacs-persist" dotemacs-cache-dir)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)
  
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-projectile
  :after treemacs projectile)
