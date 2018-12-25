;;; feature/keybinds/+emacs-bindings.el -*- lexical-binding: t; -*-

(map!
  "f" '(:ignore t :which-key "file")
  ;; file
  "ff"            #'helm-find-files)
