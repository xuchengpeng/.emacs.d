;;; feature/editor/packages.el -*- lexical-binding: t; -*-

(package! aggressive-indent
          expand-region
          multiple-cursors
          undo-tree
          smartparens
          avy)

(unless (fboundp 'display-line-numbers-mode)
  (package! nlinum))
