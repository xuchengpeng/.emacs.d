;;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

(use-package editorconfig
  :hook (dotemacs-post-init . editorconfig-mode))
