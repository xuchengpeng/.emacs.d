;;; completion/helm/config.el -*- lexical-binding: t; -*-

(use-package helm
  :defer t
  :config
  (setq helm-candidate-number-limit 50
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        ;; Default helm window sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil
        ;; disable special behavior for left/right, M-left/right keys.
        helm-ff-lynx-style-map nil)
  
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
  
  (helm-mode +1))

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

(use-package helm-descbinds
  :hook (helm-mode . helm-descbinds-mode))

