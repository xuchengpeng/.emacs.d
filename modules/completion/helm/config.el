;;; completion/helm/config.el -*- lexical-binding: t; -*-

(use-package helm
  :defer t
  :hook (pre-command . helm-mode)
  :init
  (define-key!
    [remap apropos]                     'helm-apropos
    [remap find-library]                'helm-locate-library
    [remap bookmark-jump]               'helm-bookmarks
    [remap describe-key]                'helm-descbinds
    [remap execute-extended-command]    'helm-M-x
    [remap find-file]                   'helm-find-files
    [remap imenu-anywhere]              'helm-imenu-anywhere
    [remap imenu]                       'helm-semantic-or-imenu
    [remap noop-show-kill-ring]         'helm-show-kill-ring
    [remap switch-to-buffer]            'helm-buffers-list
    [remap projectile-find-file]        'helm-projectile-find-file
    [remap projectile-recentf]          'helm-projectile-recentf
    [remap projectile-switch-project]   'helm-projectile-switch-project
    [remap projectile-switch-to-buffer] 'helm-projectile-switch-to-buffer
    [remap recentf-open-files]          'helm-recentf
    [remap yank-pop]                    'helm-show-kill-ring)
  
  (define-key! helm-map
    "TAB"           'helm-execute-persistent-action
    "<tab>"         'helm-execute-persistent-action
    "C-z"           'helm-select-action)
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
  
  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-M-x-fuzzy-match fuzzy
          helm-ag-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-mode-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy))
  
  (helm-mode +1))

(when (featurep! +fuzzy)
  (use-package helm-flx
    :hook (helm-mode . helm-flx-mode)
    :config (helm-flx-mode +1)))

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

