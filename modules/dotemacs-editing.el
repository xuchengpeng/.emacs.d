;;; dotemacs-editing.el --- Editing utils. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(avy smartparens undo-tree expand-region multiple-cursors move-text))

(use-package avy
  :defer t)

(use-package undo-tree
  :hook (find-file . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat dotemacs-cache-dir "undo-tree-hist/")))
        undo-tree-auto-save-history nil
        undo-tree-visualizer-diff t
        undo-tree-enable-undo-in-region t))

(use-package smartparens
  :hook ((find-file . smartparens-global-mode)
         ;; (prog-mode . smartparens-strict-mode)
         )
  :config
  (require 'smartparens-config))

(use-package expand-region
  :commands (er/expand-region))

(use-package multiple-cursors
  :commands (mc/mark-all-like-this mc/mark-next-like-this mc/mark-previous-like-this)
  :init
  (setq mc/list-file (concat dotemacs-cache-dir "mc-lists.el")))

(use-package move-text
  :commands (move-text-up move-text-down))

(provide 'dotemacs-editing)
;;; dotemacs-editing.el ends here