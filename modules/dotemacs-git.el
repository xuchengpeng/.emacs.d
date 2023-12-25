;;; dotemacs-git.el --- Git. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'diff-hl)

(use-package diff-hl
  :hook ((prog-mode after-save) . diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(provide 'dotemacs-git)
;;; dotemacs-git.el ends here