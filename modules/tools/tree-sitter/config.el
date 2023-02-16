;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package tree-sitter
  :defer t
  :hook (prog-mode . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs))
