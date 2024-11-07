;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4)
  (setq c-ts-mode-indent-offset 4))

(use-package reformatter
  :ensure t)

(reformatter-define lua-format
  :program "stylua"
  :args '("-"))

(reformatter-define python-format
  :program "black"
  :args '("-q" "-"))

(reformatter-define sh-format
  :program "shfmt"
  :args `("-i" ,(number-to-string sh-basic-offset) "-"))

(reformatter-define yaml-format
  :program "prettier"
  :args '("--parser" "yaml"))

(use-package markdown-mode
  :ensure t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . (lambda ()
                           (keymap-local-set dotemacs-localleader-key markdown-mode-command-map)))
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
  (keymap-set markdown-mode-command-map "i" markdown-mode-style-map)

  (reformatter-define markdown-format
    :program "prettier"
    :args '("--parser" "markdown")))

(provide 'init-lang)
;;; init-lang.el ends here
