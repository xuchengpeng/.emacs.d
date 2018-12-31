;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :diminish (js2-mode)
  :config
  (setq js2-basic-offset 2
        js2-basic-indent 2
        js2-bounce-indent-p nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (set-electric! 'js2-mode :chars '(?\} ?\) ?. ?:))
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
