;;; dotemacs-lsp-mode.el --- lsp-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(lsp-mode lsp-ui consult-lsp lsp-pyright))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode 'python-mode)
                         (lsp-deferred))))
  :init
  (setq lsp-session-file (concat dotemacs-cache-dir "lsp-session")
        lsp-server-install-dir (concat dotemacs-local-dir "lsp")
        lsp-keymap-prefix nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-keep-workspace-alive nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting nil
        lsp-lens-enable nil
        lsp-signature-auto-activate nil)
  (define-key dotemacs-leader-map "c"  '("Code" . (keymap)))
  (define-key dotemacs-leader-map "ca" '("Code Action" . lsp-execute-code-action))
  (define-key dotemacs-leader-map "cd" '("Find Definition" . lsp-find-definition))
  (define-key dotemacs-leader-map "cf" '("Format" . lsp-format-buffer))
  (define-key dotemacs-leader-map "ci" '("Find Implementation" . lsp-find-implementation))
  (define-key dotemacs-leader-map "cr" '("Find References" . lsp-find-references))
  (define-key dotemacs-leader-map "cR" '("Rename" . lsp-rename))
  (define-key dotemacs-leader-map "cs" '("File Symbols" . consult-lsp-file-symbols))
  (define-key dotemacs-leader-map "cS" '("Workspace Symbols" . consult-lsp-symbols)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :defer t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'dotemacs-lsp-mode)
;;; dotemacs-lsp-mode.el ends here
