;;; completion/helm/config.el -*- lexical-binding: t; -*-

(use-package helm
  :defer t
  :config
  (require 'helm-config)
  
  (setq helm-display-header-line nil
        helm-imenu-execute-action-at-once-if-one nil
        helm-echo-input-in-header-line t
        helm-bookmark-show-location t)
  
  (when (featurep! +fuzzy)
    (setq helm-M-x-fuzzy-match t
          helm-ag-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-bookmark-show-location t
          helm-buffers-fuzzy-matching t
          helm-completion-in-region-fuzzy-match t
          helm-ff-fuzzy-matching t
          helm-file-cache-fuzzy-match t
          helm-flx-for-helm-locate t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-locate-fuzzy-match t
          helm-mode-fuzzy-match t
          helm-projectile-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t))
  
  (let ((command
         (cond
          ((executable-find "rg")
           "rg --color=always --smart-case --no-heading --line-number %s %s %s")
          ((executable-find "ag")
           "ag --color --smart-case --nogroup %s %s %s")
          ((executable-find "pt")
           "pt --color --smart-case --nogroup --numbers %s %s %s")
          (t
           "ag --color --smart-case --nogroup %s %s %s"))))
    (setq helm-grep-ag-command command))
  
  (helm-mode +1)
  
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode +1))

(use-package helm-ag
  :after (helm)
  :config
  (let ((command
         (cond
          ((executable-find "rg")
           "rg --color never --smart-case --no-heading --line-number")
          ((executable-find "ag")
           "ag --nocolor --smart-case --nogroup")
          ((executable-find "pt")
           "pt --nocolor --smart-case --nogroup --numbers")
          (t
           "ag --nocolor --smart-case --nogroup"))))
    (setq helm-ag-base-command command)))

(use-package helm-swoop
  :after (helm)
  :config  
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)
  
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)
  
  ;; ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)
  
  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t)
  
  ;; If you prefer fuzzy matching
  (setq helm-swoop-use-fuzzy-match t)
  
  ;; If you would like to use migemo, enable helm's migemo feature
  ;; (helm-migemo-mode 1)
  )

(use-package helm-projectile
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package helm-flx
  :when (featurep! +fuzzy)
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))

(use-package helm-descbinds
  :hook (helm-mode . helm-descbinds-mode))

