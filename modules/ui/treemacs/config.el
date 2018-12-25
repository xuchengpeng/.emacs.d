;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(use-package treemacs
  :commands (treemacs)
  :config
  (setq treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-position                   'left
        treemacs-is-never-other-window      t
        treemacs-silent-refresh             nil
        treemacs-indentation                2
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-display-in-side-window     t
        treemacs-persist-file               (expand-file-name "treemacs-persist" dotemacs-cache-dir))
  
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-projectile
  :after treemacs projectile)
