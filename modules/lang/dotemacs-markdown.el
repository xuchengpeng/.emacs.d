;;; dotemacs-markdown.el --- markdown-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(markdown-mode markdown-toc))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (add-to-list 'dotemacs-localleader-mode-map-alist '(markdown-mode . markdown-mode-map))
  :config
  (define-key markdown-mode-map "o" #'markdown-open)
  (define-key markdown-mode-map "p" #'markdown-preview)
  (define-key markdown-mode-map "e" #'markdown-export)
  (define-key markdown-mode-map "i" '("Insert" . (keymap)))
  (define-key markdown-mode-map "iT" #'markdown-toc-generate-toc)
  (define-key markdown-mode-map "ib" #'markdown-insert-bold)
  (define-key markdown-mode-map "ic" #'markdown-insert-code)
  (define-key markdown-mode-map "ie" #'markdown-insert-italic)
  (define-key markdown-mode-map "if" #'markdown-insert-footnote)
  (define-key markdown-mode-map "ih" #'markdown-insert-header-dwim)
  (define-key markdown-mode-map "ii" #'markdown-insert-image)
  (define-key markdown-mode-map "ik" #'markdown-insert-kbd)
  (define-key markdown-mode-map "il" #'markdown-insert-link)
  (define-key markdown-mode-map "ip" #'markdown-insert-pre)
  (define-key markdown-mode-map "iq" #'markdown-insert-blockquote)
  (define-key markdown-mode-map "is" #'markdown-insert-strike-through)
  (define-key markdown-mode-map "it" #'markdown-insert-table)
  (define-key markdown-mode-map "iw" #'markdown-insert-wiki-link))

(provide 'dotemacs-markdown)
;;; dotemacs-markdown.el ends here
