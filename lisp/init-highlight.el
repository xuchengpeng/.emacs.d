;;; init-highlight.el --- Highlight. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(symbol-overlay hl-todo diff-hl))

(use-package symbol-overlay
  :commands symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  :init
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
  (global-set-key (kbd "M-C") 'symbol-overlay-remove-all))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1))

(provide 'init-highlight)
;;; init-highlight.el ends here
