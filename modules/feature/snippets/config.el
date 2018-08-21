;;; feature/snippets/config.el -*- lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  ;; :hook (after-init . yas-global-mode)
  :hook ((prog-mode     . yas-minor-mode)
         (snippet-mode  . yas-minor-mode)
         (markdown-mode . yas-minor-mode)
         (org-mode      . yas-minor-mode))
  :config
  (setq yas-snippet-dirs (list (concat dotemacs-dir "snippets/")))
  ;; (use-package yasnippet-snippets :ensure t)
  (yas-reload-all))
