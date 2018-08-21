;;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))
