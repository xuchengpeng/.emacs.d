;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/lang" dotemacs-dir) load-path)
(require 'init-markdown)

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4))

(when (dotemacs-treesit-available-p)
  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4)))

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

(provide 'init-lang)
;;; init-lang.el ends here
