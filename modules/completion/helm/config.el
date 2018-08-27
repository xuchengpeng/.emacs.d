;;; completion/helm/config.el -*- lexical-binding: t; -*-

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list))
  :config
  (require 'helm-config)
  
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  
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
           "ag --color --smart-case --no-heading %s %s %s")
          ((executable-find "pt")
           "pt --color --smart-case --nogroup --numbers %s %s %s")
          (t helm-grep-ag-command))))
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
           "ag --nocolor --smart-case --no-heading")
          ((executable-find "pt")
           "pt --nocolor --smart-case --nogroup --numbers")
          (t helm-ag-base-command))))
    (setq helm-ag-base-command command)))

(use-package helm-swoop
  :after (helm)
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
  
  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
  
  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  
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
  :after (helm)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package helm-flx
  :when (featurep! +fuzzy)
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))

;; (use-package helm-descbinds
;;   :hook (helm-mode . helm-descbinds-mode))

