;;; dotemacs-editing.el --- Editing utils.
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(avy smartparens undo-tree expand-region multiple-cursors))

(use-package avy
  :defer t)

(use-package undo-tree
  :hook (find-file . global-undo-tree-mode)
  :init
  (add-hook 'undo-tree-visualizer-mode-hook (lambda () (display-line-numbers-mode -1)))
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat dotemacs-cache-dir "undo-tree-hist/")))
        undo-tree-auto-save-history t))

(use-package smartparens
  :hook ((find-file . smartparens-global-mode)
         ;; (prog-mode . smartparens-strict-mode)
         )
  :config
  (require 'smartparens-config))

(use-package expand-region
  :defer t)

(use-package multiple-cursors
  :defer t
  :init
  (setq mc/list-file (concat dotemacs-cache-dir "mc-lists.el")))

(provide 'dotemacs-editing)
;;; dotemacs-editing.el ends here