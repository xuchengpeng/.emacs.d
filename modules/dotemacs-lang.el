;;; dotemacs-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lang/" dotemacs-modules-dir))

(require 'dotemacs-markdown)
(require 'dotemacs-lua)

(provide 'dotemacs-lang)
;;; dotemacs-lang.el ends here
