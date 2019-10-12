;;; editor/utils/config.el -*- lexical-binding: t; -*-

;; Increase selected region by semantic units
(use-package expand-region
  :defer t)

;; Multiple cursors
(use-package multiple-cursors
  :defer t
  :init
  (setq mc/list-file (concat dotemacs-cache-dir ".mc-lists.el")))

;; Treat undo history as a tree
(use-package undo-tree
  :hook (find-file . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat dotemacs-cache-dir "undo-tree-hist/")))
        undo-tree-auto-save-history t))

(use-package smartparens
  :hook ((find-file . smartparens-global-mode)
         ;; (prog-mode . smartparens-strict-mode)
         )
  :config
  (require 'smartparens-config))

(use-package avy
  :defer t)
