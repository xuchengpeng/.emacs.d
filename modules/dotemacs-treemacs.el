;;; dotemacs-treemacs.el --- File tree. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; File tree configuration.
;;

;;; Code:

(dotemacs-require-packages '(treemacs treemacs-projectile))

(use-package treemacs
  :defer t
  :commands (treemacs)
  :init
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  :config
  (setq treemacs-follow-after-init     t
        treemacs-is-never-other-window t
        treemacs-no-png-images         t
        treemacs-sorting               'alphabetic-case-insensitive-asc
        treemacs-persist-file          (concat dotemacs-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat dotemacs-cache-dir "treemacs-last-error-persist"))
  
  ;; Don't follow the cursor
  (treemacs-follow-mode -1))

(use-package treemacs-projectile
  :after treemacs projectile)

(provide 'dotemacs-treemacs)
;;; dotemacs-treemacs.el ends here