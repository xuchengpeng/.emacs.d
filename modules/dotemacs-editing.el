;;; dotemacs-editing.el --- Editing utils. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(avy expand-region multiple-cursors move-text vundo))

(use-package avy
  :defer t)

(use-package expand-region
  :commands (er/expand-region))

(use-package multiple-cursors
  :commands (mc/mark-all-like-this mc/mark-next-like-this mc/mark-previous-like-this)
  :init
  (setq mc/list-file (concat dotemacs-cache-dir "mc-lists.el")))

(use-package move-text
  :commands (move-text-up move-text-down))

(use-package vundo
  :bind ("C-x u" . vundo))

(provide 'dotemacs-editing)
;;; dotemacs-editing.el ends here
