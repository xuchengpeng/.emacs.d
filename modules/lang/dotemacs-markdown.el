;;; dotemacs-markdown.el --- markdown-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'markdown-mode)

(defun dotemacs-markdown-toc ()
  "Extract headings from the current Markdown buffer.
The generated and indented TOC will be inserted at point."
  (interactive)
  (let (toc-list markdown-toc)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(#+\\)\\s-+\\(.*\\)" nil t)
        (let* ((level (length (match-string 1)))
               (heading-text (match-string 2))
               (heading-id (downcase (replace-regexp-in-string "[[:space:]]+" "-" heading-text))))
          ;; Remove commas, parentheses from the heading-id
          (setq heading-id (replace-regexp-in-string "[,()]" "" heading-id))
          (push (cons level (cons heading-text heading-id)) toc-list))))
    (setq toc-list (reverse toc-list))
    (dolist (item toc-list)
      (let* ((level (car item))
             (heading-text (cadr item))
             (heading-id (cddr item))
             (indentation (make-string (* 2 (1- level)) ?\ ))
             (line (format "- [%s](#%s)\n" heading-text heading-id)))
        (setq markdown-toc (concat markdown-toc (concat indentation line)))))
    (insert markdown-toc)))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (add-to-list 'dotemacs-major-mode-map-alist '(markdown-mode . markdown-mode-command-map))
  :config
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-whole-heading-line t
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                "<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
  (when (executable-find "pandoc")
    (setq markdown-command '("pandoc" "--from=markdown" "--to=html5" "--mathjax" "--highlight-style=pygments")))
  (define-key markdown-mode-command-map "i" markdown-mode-style-map))

(provide 'dotemacs-markdown)
;;; dotemacs-markdown.el ends here
