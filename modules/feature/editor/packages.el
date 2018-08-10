;;; feature/editor/packages.el

(package! aggressive-indent
          expand-region
          multiple-cursors
          undo-tree
          smartparens
          avy)

(unless (fboundp 'display-line-numbers-mode)
  (package! nlinum))
