;;; feature/highlight/config.el -*- lexical-binding: t; -*-

;; Highlight symbols
;; (use-package highlight-symbol
;;   :diminish highlight-symbol-mode
;;   :commands (highlight-symbol-mode)
;;   :hook (prog-mode . highlight-symbol-mode)
;;   :config
;;   (setq highlight-symbol-idle-delay 0.5))

;; Highlight symbols
(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :commands symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :commands (rainbow-mode))

;; Highlight uncommitted changes
(use-package diff-hl
  :diminish diff-hl-mode
  :hook ((prog-mode after-save) . diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
