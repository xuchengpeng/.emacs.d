;;; ui/highlight/config.el -*- lexical-binding: t; -*-

;; Highlight symbols
;; (use-package highlight-symbol
;;   :commands (highlight-symbol-mode)
;;   :hook (prog-mode . highlight-symbol-mode)
;;   :config
;;   (setq highlight-symbol-idle-delay 0.5))

;; Highlight symbols
(use-package symbol-overlay
  :commands symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode))

;; Colorize color names in buffers
(use-package rainbow-mode
  :commands (rainbow-mode))

;; Highlight uncommitted changes
(use-package diff-hl
  :hook ((prog-mode after-save) . diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))
