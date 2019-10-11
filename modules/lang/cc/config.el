;;; lang/cc/config.el -*- lexical-binding: t; -*-

(defvar +clang-format-file (concat (dir!) "/clang-format.el"))

;; c
(set-electric! '(c-mode c++-mode objc-mode java-mode) :chars '(?\n ?\}))
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq c-basic-offset 4
                  default-tab-width 4
                  tab-width 4
                  indent-tabs-mode nil)
            (load +clang-format-file t (not dotemacs-debug-mode))))
