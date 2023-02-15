;;; lang/lua/config.el -*- lexical-binding: t; -*-

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :hook (lua-mode . lsp-deferred))
