;;; feature/snippets/config.el -*- lexical-binding: t; -*-

(defvar +snippets-dir (concat (DIR!) "snippets/"))

(use-package yasnippet
  :diminish yas-minor-mode
  ;; :hook (after-init . yas-global-mode)
  :hook ((prog-mode     . yas-minor-mode)
         (snippet-mode  . yas-minor-mode)
         (markdown-mode . yas-minor-mode)
         (org-mode      . yas-minor-mode))
  :config
  (setq yas-snippet-dirs (list +snippets-dir))
  ;; (use-package yasnippet-snippets :ensure t)
  (yas-reload-all))
