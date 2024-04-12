;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'reformatter)

(push (expand-file-name "lisp/lang" dotemacs-dir) load-path)
(require 'init-markdown)
(require 'init-lua)

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4))

(when (dotemacs-treesit-available-p)
  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4)))

(with-eval-after-load 'python
  (reformatter-define python-format
    :program "black"
    :args '("-q" "-")))

(with-eval-after-load 'sh-script
  (reformatter-define shfmt
    :program "shfmt"
    :args `("-i" ,(number-to-string sh-basic-offset) "-")))

(with-eval-after-load 'yaml-ts-mode
  (reformatter-define yaml-format
    :program "prettier"
    :args '("--parser" "yaml")))

(provide 'init-lang)
;;; init-lang.el ends here
