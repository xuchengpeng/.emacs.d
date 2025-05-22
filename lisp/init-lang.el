;;; init-lang.el --- Programming languages  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4)
  (setq c-ts-mode-indent-offset 4))

(use-package reformatter
  :ensure t
  :after (:any python sh-script json-mode markdown-mode)
  :config
  (reformatter-define python-format
    :program "black"
    :args '("-q" "-"))
  (reformatter-define sh-format
    :program "shfmt"
    :args `("-i" ,(number-to-string sh-basic-offset) "-"))
  (reformatter-define json-format
    :program "prettier"
    :args '("--parser" "json"))
  (reformatter-define markdown-format
    :program "prettier"
    :args '("--parser" "markdown")))

(use-package markdown-mode
  :ensure t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :config
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-whole-heading-line t
        markdown-fontify-code-blocks-natively t
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 900px; width: 100%; margin: 0 auto; padding: 0 10px; } </style>"
                "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css' />"
                "<script defer src='https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js'></script>"
                "<script defer src='https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/contrib/auto-render.min.js' onload='renderMathInElement(document.body);'></script>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>hljs.highlightAll();</script>"))
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5" "--katex" "--highlight-style=pygments")))

(provide 'init-lang)
;;; init-lang.el ends here
