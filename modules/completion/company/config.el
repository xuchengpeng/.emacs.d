;;; completion/company/config.el -*- lexical-binding: t; -*-

(use-package company
  :commands (company-complete-common company-manual-begin company-grab-line)
  :hook ((pre-command . global-company-mode)
         (find-file . global-company-mode))
  :init
  (map-local!
    :keymaps        'company-mode-map
    "C-@"           'company-complete
    "C-x s"         'company-ispell
    "C-x C-f"       'company-files
    "C-x C-o"       'company-capf
    "C-x C-s"       'company-yasnippet)

  (define-key! company-active-map
    "C-p"           'company-select-previous
    "C-n"           'company-select-next
    "TAB"           'company-complete-common-or-cycle
    "<tab>"         'company-complete-common-or-cycle
    "S-TAB"         'company-select-previous
    "<backtab>"     'company-select-previous)
  
  (define-key! company-search-map
    "C-p"           'company-select-previous
    "C-n"           'company-select-next)
  :config  
  (setq company-idle-delay 0.2
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  
  (add-hook 'company-mode-hook #'+company-init-backends-h)
  
  (global-company-mode +1))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-save-file (concat dotemacs-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))
