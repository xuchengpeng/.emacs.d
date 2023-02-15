;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-session-file (concat dotemacs-cache-dir "lsp-session")
        lsp-server-install-dir (concat dotemacs-cache-dir "lsp")))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :defer t
  :when (modulep! :completion vertico)
  :init
  (define-key! lsp-mode-map
    [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package helm-lsp
  :when (modulep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)

(use-package lsp-ivy
  :when (modulep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
