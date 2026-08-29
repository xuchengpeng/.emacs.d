;;; init-programming.el --- programming  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elisp-mode
  :ensure nil
  :custom
  (elisp-fontify-semantically t))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :init
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-margin-indicator-position 'right-margin))

(use-package flyspell
  :defer t
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together")
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'lisp-data-mode 'makefile-mode 'snippet-mode)
                         (eglot-ensure))))
  :init
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-code-action-indications '(eldoc-hint)))

(use-package treesit
  :ensure nil
  :custom
  (treesit-enabled-modes t))

(use-package apheleia
  :ensure t
  :commands apheleia-format-buffer
  :hook (prog-mode . apheleia-mode))

(use-package colorful-mode
  :ensure t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :hook (after-init . global-colorful-mode))

(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4)
  (setq c-ts-indent-offset 4))

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

(provide 'init-programming)
;;; init-programming.el ends here
