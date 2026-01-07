;;; init-lang.el --- Programming languages  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4)
  (setq c-ts-mode-indent-offset 4))

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
                "<style> *, ::after, ::before, ::backdrop, ::file-selector-button { box-sizing: border-box; margin: 0; padding: 0; border: 0 solid; } body { max-width: 72rem; width: 100%; margin: 0 auto; padding: 1em; } </style>"
                "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.27/dist/katex.min.css\" integrity=\"sha384-Pu5+C18nP5dwykLJOhd2U4Xen7rjScHN/qusop27hdd2drI+lL5KvX7YntvT8yew\" crossorigin=\"anonymous\">"
                "<script type=\"module\">
  import renderMathInElement from \"https://cdn.jsdelivr.net/npm/katex@0.16.27/dist/contrib/auto-render.mjs\";
  renderMathInElement(document.body, {
    delimiters: [
      {left: '$$', right: '$$', display: true},
      {left: '$', right: '$', display: false}
    ],
  });
</script>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>hljs.highlightAll();</script>")
        markdown-command "marked"))

(provide 'init-lang)
;;; init-lang.el ends here
