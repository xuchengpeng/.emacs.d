;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :diminish (js2-mode)
  :config
  (setq-default js2-basic-offset 2
                js2-basic-indent 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
