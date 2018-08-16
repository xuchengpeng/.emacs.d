;;; feature/snippets/config.el -*- lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs (list (concat dotemacs-dir "snippets/")))
  ;; (use-package yasnippet-snippets :ensure t)
  )
