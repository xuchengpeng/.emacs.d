;;; init-highlight.el --- Highlight. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(symbol-overlay hl-todo diff-hl))

(use-package symbol-overlay
  :commands symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (keymap-set symbol-overlay-mode-map "M-i" 'symbol-overlay-put)
  (keymap-set symbol-overlay-mode-map "M-I" 'symbol-overlay-remove-all)
  (keymap-set symbol-overlay-mode-map "M-n" 'symbol-overlay-jump-next)
  (keymap-set symbol-overlay-mode-map "M-p" 'symbol-overlay-jump-prev))

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
