;;; init-programming.el --- programming  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
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
        eglot-send-changes-idle-time 0.5))

;; treesit support
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (sh-mode . bash-ts-mode)
          (python-mode . python-ts-mode)))
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (when (treesit-language-available-p 'elisp)
                                      (treesit-parser-create 'elisp)))))

(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1))

(defvar +highlight-keywords
  '(("\\<\\(TODO\\|FIXME\\|BUG\\)\\>" 1 'error prepend)
    ("\\<\\(NOTE\\|HACK\\|MAYBE\\)\\>" 1 'warning prepend)))

(define-minor-mode +highlight-keywords-mode
  "Highlight keywords, like TODO, FIXME..."
  :global nil
  (if +highlight-keywords-mode
      (font-lock-add-keywords nil +highlight-keywords)
    (font-lock-remove-keywords nil +highlight-keywords))

  ;; Fontify the current buffer
  (when (bound-and-true-p font-lock-mode)
    (font-lock-flush)))

(add-hook 'prog-mode-hook #'+highlight-keywords-mode)

(provide 'init-programming)
;;; init-programming.el ends here
