;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'reformatter)

(push (expand-file-name "lisp/lang" dotemacs-dir) load-path)
(require 'init-markdown)

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4))

(when (dotemacs-treesit-available-p)
  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4))

  (when (fboundp 'lua-ts-mode)
    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
    (with-eval-after-load 'lua-ts-mode
      (setq lua-ts-indent-offset 2))))

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

(provide 'init-lang)
;;; init-lang.el ends here
