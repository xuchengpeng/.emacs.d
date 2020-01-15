;;; completion/ivy/config.el -*- lexical-binding: t -*-

(use-package ivy
  :defer t
  :hook (pre-command . ivy-mode)
  :init
  (let ((standard-search-fn
         (if (featurep! +prescient)
             #'+ivy-prescient-non-fuzzy
           #'ivy--regex-plus))
        (alt-search-fn
         (if (featurep! +fuzzy)
             #'ivy--regex-fuzzy
           ;; Ignore order for non-fuzzy searches by default
           #'ivy--regex-ignore-order)))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  :config
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)
  ;; Integration with `magit'
  (after-load! 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  (ivy-mode +1))

(use-package flx
  :when (featurep! +fuzzy)
  :unless (featurep! +prescient)
  :defer t  ; is loaded by ivy
  :init (setq ivy-flx-limit 10000))

(use-package ivy-prescient
  :if (featurep! +prescient)
  :hook (ivy-mode . ivy-prescient-mode)
  :init
  (setq prescient-filter-method
        (if (featurep! +fuzzy)
            '(literal regexp initialism fuzzy)
          '(literal regexp initialism))
        ivy-prescient-retain-classic-highlighting t)

  :config
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat dotemacs-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (dolist (cmd '(ivy-switch-buffer-other-window
                 counsel-projectile-switch-to-buffer))
    (ivy-set-display-transformer cmd 'ivy-rich--ivy-switch-buffer-transformer)))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package swiper
  :defer t)

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :init
  (define-key!
    [remap switch-to-buffer]            'ivy-switch-buffer
    [remap imenu-anywhere]              'ivy-imenu-anywhere
    [remap apropos]                     'counsel-apropos
    [remap bookmark-jump]               'counsel-bookmark
    [remap describe-face]               'counsel-faces
    [remap describe-function]           'counsel-describe-function
    [remap describe-key]                'counsel-descbinds
    [remap describe-variable]           'counsel-describe-variable
    [remap execute-extended-command]    'counsel-M-x
    [remap find-file]                   'counsel-find-file
    [remap find-library]                'counsel-find-library
    [remap info-lookup-symbol]          'counsel-info-lookup-symbol
    [remap imenu]                       'counsel-imenu
    [remap recentf-open-files]          'counsel-recentf
    [remap org-capture]                 'counsel-org-capture
    [remap swiper]                      'counsel-grep-or-swiper
    [remap evil-ex-registers]           'counsel-evil-registers
    [remap yank-pop]                    'counsel-yank-pop
    [remap compile]                     '+ivy/compile
    [remap projectile-find-file]        'counsel-projectile-find-file
    [remap projectile-find-dir]         'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             'counsel-projectile-grep
    [remap projectile-ag]               'counsel-projectile-ag
    [remap projectile-switch-project]   'counsel-projectile-switch-project
    [remap projectile-compile-project]  '+ivy/project-compile)
  
  (map-local!
    :keymaps        'counsel-mode-map
    "s"             'counsel-grep-or-swiper
    "C-x C-r"       'ivy-resume)
  
  (define-key! ivy-minibuffer-map
    "TAB"           'ivy-partial-or-done
    "RET"           'ivy-alt-done)
  :config
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg --smart-case --no-heading --line-number --color never %s %s"
          counsel-git-command "rg --files"))
  (setq counsel-rg-base-command "rg --smart-case --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag --smart-case --nocolor --nogroup --numbers %s"
        counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")

  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))
  
  ;; Make `counsel-compile' projectile-aware (if you prefer it over
  ;; `+ivy/compile' and `+ivy/project-compile')
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  
  (after! savehist
    ;; Persist `counsel-compile' history
    (add-to-list 'savehist-additional-variables 'counsel-compile-history)))

(use-package counsel-projectile
  :commands (counsel-projectile-find-file counsel-projectile-find-dir counsel-projectile-switch-to-buffer
             counsel-projectile-grep counsel-projectile-ag counsel-projectile-switch-project)
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (counsel-projectile-mode))

;; (use-package smex
;;   :defer t
;;   :config
;;   (setq smex-history-length 10
;;         smex-save-file (concat dotemacs-cache-dir "smex-items")))

;;;###package amx
(setq amx-save-file (concat dotemacs-cache-dir "amx-items"))  ; used by `counsel-M-x'

(use-package ivy-xref
  :after xref
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
