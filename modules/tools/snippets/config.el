;;; tools/snippets/config.el

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (concat dotemacs-dir "snippets/")))
  ;; (use-package yasnippet-snippets :ensure t)
  (yas-global-mode))
