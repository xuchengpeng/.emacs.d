;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(defvar +markdown-compile-functions
  '(+markdown-compile-marked
    +markdown-compile-pandoc
    +markdown-compile-markdown
    +markdown-compile-multimarkdown)
  "A list of commands to try when attempting to build a markdown file with
`markdown-open' or `markdown-preview', stopping at the first one to return non-nil.

Each function takes three argument. The beginning position of the region to
capture, the end position, and the output buffer.")

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-gfm-uppercase-checkbox t ; for compat with org-mode
        markdown-command #'+markdown-compile
        markdown-open-command
        (cond (IS-MAC "open")
              (IS-LINUX "xdg-open")))
  (map-local! markdown-mode-map
    "o"  '(markdown-open :which-key "open")
    "p"  '(markdown-preview :which-key "preview")
    "e"  '(markdown-export :which-key "export")
    "i"  '(:ignore t :which-key "insert")
    "it" '(markdown-toc-generate-toc :which-key "generate toc")
    "ii" '(markdown-insert-image :which-key "insert image")
    "il" '(markdown-insert-link :which-key "insert link")
    "i*" '(markdown-insert-list-item :which-key "insert list item")
    "ib" '(markdown-insert-bold :which-key "insert bold")
    "iI" '(markdown-insert-italic :which-key "insert italic")))

;; turn on visual-line-mode
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook))
  (add-hook hook 'visual-line-mode))

(use-package markdown-toc
  :commands(markdown-toc-generate-toc
            markdown-toc-refresh-toc
            markdown-toc-generate-or-refresh-toc))
