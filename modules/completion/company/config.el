;;; completion/company/config.el -*- lexical-binding: t; -*-

(use-package company
  :diminish company-mode
  :bind (("M-/" . company-complete)
          ("C-c C-y" . company-yasnippet)
          :map company-active-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next)
          ("TAB" . company-complete-common-or-cycle)
          ("<tab>" . company-complete-common-or-cycle)
          ("S-TAB" . company-select-previous)
          ("<backtab>" . company-select-previous)
          :map company-search-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-require-match nil
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode eshell-mode))
  
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
