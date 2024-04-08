;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/lang" dotemacs-dir) load-path)

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4))

(when (dotemacs-treesit-available-p)
  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4)))

(require 'init-markdown)
(require 'init-python)
(require 'init-lua)

(provide 'init-lang)
;;; init-lang.el ends here
