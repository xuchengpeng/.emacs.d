;;; init-programming.el --- programming. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'eldoc-box)

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

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
        eglot-events-buffer-size 0)
  :config
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode)))

;; treesit support
(when (dotemacs-treesit-available-p)
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (go-mod-mode . go-mod-ts-mode)
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

(provide 'init-programming)
;;; init-programming.el ends here
