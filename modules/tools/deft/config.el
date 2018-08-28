;;; tools/deft/config.el -*- lexical-binding: t; -*-

(use-package deft
  :commands deft
  :config
  (setq deft-directory (concat dotemacs-dir "org/")
        deft-extensions '("org" "md" "txt")
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))))
