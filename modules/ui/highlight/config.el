;;; ui/highlight/config.el -*- lexical-binding: t; -*-

;; Highlight symbols
;; (use-package highlight-symbol
;;   :diminish highlight-symbol-mode
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
