;;; lang/markdown/config.el -*- lexical-binding: t; -*-

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

;; turn on visual-line-mode
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook))
  (add-hook hook 'visual-line-mode))

(use-package markdown-toc
  :commands(markdown-toc-generate-toc
            markdown-toc-refresh-toc
            markdown-toc-generate-or-refresh-toc))
