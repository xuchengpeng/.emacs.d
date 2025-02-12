;;; init-programming.el --- programming. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package symbol-overlay
  :ensure t
  :commands symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (keymap-set symbol-overlay-mode-map "M-i" 'symbol-overlay-put)
  (keymap-set symbol-overlay-mode-map "M-I" 'symbol-overlay-remove-all)
  (keymap-set symbol-overlay-mode-map "M-n" 'symbol-overlay-jump-next)
  (keymap-set symbol-overlay-mode-map "M-p" 'symbol-overlay-jump-prev))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1))

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

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-rewrite))

(provide 'init-programming)
;;; init-programming.el ends here
