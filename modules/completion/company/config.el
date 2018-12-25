;;; completion/company/config.el -*- lexical-binding: t; -*-

(use-package company
  :diminish company-mode
  :commands (company-complete-common company-manual-begin company-grab-line)
  :hook (prog-mode . global-company-mode)
  :config  
  (setq company-idle-delay 0.2
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-require-match nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode))
  
  (setq company-backends (mapcar #'+company/backend-with-yas company-backends))
  
  (global-company-mode +1))
