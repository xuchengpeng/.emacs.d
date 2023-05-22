;;; dotemacs-markdown.el --- markdown-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(markdown-mode markdown-toc))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (add-to-list 'dotemacs-localleader-mode-map-alist '(markdown-mode . markdown-mode-style-map)))

(provide 'dotemacs-markdown)
;;; dotemacs-markdown.el ends here
