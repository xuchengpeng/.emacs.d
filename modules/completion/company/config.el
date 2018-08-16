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
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (dotemacs-post-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-require-match nil)
  
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
