;;; dotemacs-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lang/" dotemacs-modules-dir))

(require 'dotemacs-org)
(require 'dotemacs-markdown)
(require 'dotemacs-lua)
(require 'dotemacs-python)

(provide 'dotemacs-lang)
;;; dotemacs-lang.el ends here
