;;; feature/snippets/config.el -*- lexical-binding: t; -*-

(defvar +snippets-dir (concat dotemacs-vendor-dir "snippets/")
  "Directory where `yasnippet' will search for your private snippets.")

(use-package yasnippet
  :diminish yas-minor-mode
  ;; :hook (after-init . yas-global-mode)
  :hook ((prog-mode snippet-mode markdown-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs (list +snippets-dir))
  ;; (use-package yasnippet-snippets :ensure t)
  (yas-reload-all))
