;;; dotemacs-tree-sitter.el --- tree-sitter

(dotemacs-require-packages '(tree-sitter tree-sitter-langs))

(use-package tree-sitter
  :hook (prog-mode . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs))

(provide 'dotemacs-tree-sitter)
;;; dotemacs-tree-sitter.el ends here