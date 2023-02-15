;;; tools/lsp/packages.el -*- lexical-binding: t; -*-

(package! lsp-mode)
(package! lsp-ui)
(when (modulep! :completion vertico)
  (package! consult-lsp))
(when (modulep! :completion helm)
  (package! helm-lsp))
(when (modulep! :completion ivy)
  (package! lsp-ivy))
