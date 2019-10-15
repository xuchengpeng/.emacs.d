;;; lang/sh/config.el -*- lexical-binding: t; -*-

(use-package sh-script
  :mode (("\\.zunit\\'" . sh-mode)
         ("/bspwmrc\\'" . sh-mode))
  :config
  (set-electric! 'sh-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  
  (setq sh-indent-after-continuation 'always)

  ;; [pedantry intensifies]
  (setq-hook! 'sh-mode-hook mode-name "sh")
  
  ;; recognize function names with dashes in them
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1))))

(when (featurep! :completion company)
  (use-package company-shell
    :after sh-script
    :config
    (set-company-backend! 'sh-mode '(company-shell company-files))
    (setq company-shell-delete-duplicates t)))
