;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/lang" dotemacs-dir) load-path)

(require 'init-markdown)
(require 'init-lua)

(provide 'init-lang)
;;; init-lang.el ends here
