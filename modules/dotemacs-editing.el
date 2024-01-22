;;; dotemacs-editing.el --- Editing utils. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(avy expand-region multiple-cursors move-text vundo))

(use-package avy
  :commands (avy-goto-line))

(use-package expand-region
  :commands (er/expand-region)
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package multiple-cursors
  :commands (mc/mark-all-like-this mc/mark-next-like-this mc/mark-previous-like-this)
  :init
  (setq mc/list-file (concat dotemacs-cache-dir "mc-lists.el"))
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package move-text
  :commands (move-text-up move-text-down)
  :init
  (global-set-key [M-up] 'move-text-up)
  (global-set-key [M-down] 'move-text-down))

(use-package vundo
  :commands vundo
  :init
  (global-set-key (kbd "C-x u") 'vundo))

(provide 'dotemacs-editing)
;;; dotemacs-editing.el ends here
