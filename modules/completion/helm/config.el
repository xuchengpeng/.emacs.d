;;; completion/helm/config.el -*- lexical-binding: t; -*-

(use-package helm
  :defer t
  :hook (pre-command . helm-mode)
  :init
  (define-key!
    [remap apropos]                     'helm-apropos
    [remap find-library]                'helm-locate-library
    [remap bookmark-jump]               'helm-bookmarks
    [remap execute-extended-command]    'helm-M-x
    [remap find-file]                   'helm-find-files
    [remap locate]                      'helm-locate
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
    "[tab]"         'helm-execute-persistent-action
    "C-z"           'helm-select-action)
  :config
  (setq helm-candidate-number-limit 50
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Default helm window sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil
        ;; disable special behavior for left/right, M-left/right keys.
        helm-ff-lynx-style-map nil)
  
  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy))
  
  (when (executable-find "rg")
    (setq helm-grep-ag-command "rg --color=always --smart-case --line-number --no-heading %s %s %s"))

  (helm-mode +1))

(use-package helm-flx
  :when (featurep! +fuzzy)
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))

(use-package helm-projectile
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package helm-xref
  :after xref
  :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs))

(use-package helm-org
  :when (featurep! :lang org)
  :defer t
  :init
  (after! helm-mode
    (pushnew! helm-completing-read-handlers-alist
              '(org-capture . helm-org-completing-read-tags)
              '(org-set-tags . helm-org-completing-read-tags))))
