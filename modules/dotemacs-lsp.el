;;; dotemacs-lsp.el --- lsp-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(lsp-mode lsp-ui consult-lsp))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-session-file (concat dotemacs-cache-dir "lsp-session")
        lsp-server-install-dir (concat dotemacs-local-dir "lsp")
        lsp-keymap-prefix nil
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-segments '(count name)
        lsp-keep-workspace-alive nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :defer t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'dotemacs-lsp)
;;; dotemacs-lsp.el ends here
