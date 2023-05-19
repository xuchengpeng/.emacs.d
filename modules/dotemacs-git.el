;;; dotemacs-git.el --- Git. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(magit diff-hl))

(use-package magit
  :commands magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)
  (setq transient-levels-file  (concat dotemacs-cache-dir "transient/levels")
        transient-values-file  (concat dotemacs-cache-dir "transient/values")
        transient-history-file (concat dotemacs-cache-dir "transient/history")))

(use-package diff-hl
  :hook ((prog-mode after-save) . diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'dotemacs-git)
;;; dotemacs-git.el ends here