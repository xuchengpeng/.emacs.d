;;; dotemacs-lsp.el --- LSP.

(dotemacs-require-packages '(lsp-mode lsp-ui consult-lsp))

(use-package lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-session-file (concat dotemacs-cache-dir "lsp-session")
        lsp-server-install-dir (concat dotemacs-cache-dir "lsp")))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :defer t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] 'consult-lsp-symbols))

(provide 'dotemacs-lsp)
;;; dotemacs-lsp.el ends here