;;; lang/markdown/config.el

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; turn on visual-line-mode
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook))
  (add-hook hook 'visual-line-mode))
