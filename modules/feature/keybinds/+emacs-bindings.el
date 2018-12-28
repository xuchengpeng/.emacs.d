;;; feature/keybinds/+emacs-bindings.el -*- lexical-binding: t; -*-

(map!
  "/"       '(:ignore t :which-key "search")
  "b"       '(:ignore t :which-key "buffer")
  "c"       '(:ignore t :which-key "code")
  "f"       '(:ignore t :which-key "file")
  "g"       '(:ignore t :which-key "git")
  "h"       '(:ignore t :which-key "help")
  "i"       '(:ignore t :which-key "insert")
  "o"       '(:ignore t :which-key "open")
  "p"       '(:ignore t :which-key "project")
  "q"       '(:ignore t :which-key "quit")
  "s"       '(:ignore t :which-key "snippets")
  "t"       '(:ignore t :which-key "toggle")
  
  ;; search
  "/b"      'swiper
  "/i"      'imenu
  "/l"      'ace-link
  "/p"      'projectile-ripgrep
  
  ;; buffer
  "bb"      'switch-to-buffer
  "bk"      'kill-this-buffer
  "bn"      'next-buffer
  "bo"      'dotemacs/kill-other-buffers
  "bp"      'previous-buffer
  "bs"      'save-buffer
  
  ;; code
  "ca"      'hydra-avy/body
  "cf"      'clang-format
  "cF"      'hydra-flycheck/body
  "cm"      'hydra-multiple-cursors/body
  
  ;; file
  "fa"      'projectile-find-other-file
  "fd"      'dired
  "ff"      'find-file
  "fr"      'recentf-open-files
  "fs"      'save-buffer
  "fp"      'projectile-find-file
  
  ;; git
  "gb"      'magit-blame-addition
  "gc"      'magit-commit
  "gd"      'magit-dispatch-popup
  "gf"      'magit-find-file
  "gx"      'magit-file-delete
  "gh"      'magithub-dispatch-popup
  "gi"      'magit-init
  "gl"      'magit-log-buffer-file
  "gL"      'magit-list-repositories
  "gs"      'magit-status
  "gS"      'magit-stage-file
  "gU"      'magit-unstage-file
  "gp"      'magit-push-file
  "gP"      'magit-pull-popup
  
  ;; help
  "hF"      'describe-face
  "hM"      'describe-mode
  "hc"      'describe-char
  "hf"      'describe-function
  "hi"      'info-lookup-symbol
  "hk"      'describe-key
  "hl"      'find-library
  "hv"      'describe-variable
  
  ;; insert
  "iy"      'yank-pop
  "is"      'yas-insert-snippet
  
  ;; open
  "oa"      'org-agenda
  "oc"      'org-capture
  "ow"      'hydra-window/body
  
  ;; project
  "pc"      'projectile-compile-project
  "pf"      'projectile-find-file
  "pi"      'projectile-invalidate-cache
  "po"      'projectile-find-other-file
  "pp"      'projectile-switch-project
  "pr"      'projectile-recentf
  "ps"      '(:ignore t :which-key "search")
  "psg"     'projectile-grep
  "psa"     'projectile-ag
  "psr"     'projectile-ripgrep
  
  ;; quit
  "qq"      'evil-quit-all
  "qQ"      'evil-save-and-quit
  "qR"      'restart-emacs
  
  ;; snippets
  "s/"      'yas-visit-snippet-file
  "sn"      'yas-new-snippet
  "si"      'yas-insert-snippet
  "sr"      'yas-reload-all
  
  ;; toggle
  "ta"      'aggressive-indent-mode
  "tf"      'flycheck-mode
  "tF"      'toggle-frame-fullscreen
  "ti"      'highlight-indentation-mode
  "tI"      'highlight-indentation-current-column-mode
  "tl"      'display-line-numbers-mode
  "to"      'org-mode
  "ts"      'smartparens-mode)

(define-key!
  "M-x"             'execute-extended-command
  "M-y"             'yank-pop
  "C-s"             'swiper
  "C-r"             'swiper
  "C-x C-f"         'find-file
  "C-x r b"         'bookmark-jump
  
  "C-=" (when (featurep! :feature editor)
                    'er/expand-region)
  
  "<f8>" (cond
           ((featurep! :ui neotree)
                    'neotree-toggle)
           ((featurep! :ui treemacs)
                    'treemacs)))

(define-key! projectile-mode-map
  "C-c p"           'projectile-command-map)

(define-key! prog-mode-map
  "C-c h"           'hs-toggle-hiding)

(define-key! eshell-mode-map
  "C-c s"           '+eshell/search-history)

;;
;; company

(when (featurep! :completion company)
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
    "C-n"           'company-select-next))

;;
;; helm

(when (featurep! :completion helm)
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
    [remap yank-pop]                    'helm-show-kill-ring
    [remap swiper]                      'swiper-helm)
  
  (map-local!
    :keymaps        'helm-map
    "s"             'swiper-helm)
  
  (define-key! helm-map
    "TAB"           'helm-execute-persistent-action
    "<tab>"         'helm-execute-persistent-action
    "C-z"           'helm-select-action)
  
  (define-key! isearch-mode-map
    "M-i"           'helm-swoop-from-isearch)
  
  (define-key! helm-swoop-map
    "M-i"           'helm-multi-swoop-all-from-helm-swoop
    "M-m"           'helm-multi-swoop-current-mode-from-helm-swoop
    "C-r"           'helm-previous-line
    "C-s"           'helm-next-line)
  
  (define-key! helm-multi-swoop-map
    "C-r"           'helm-previous-line
    "C-s"           'helm-next-line))

;;
;; ivy

(when (featurep! :completion ivy)
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
    [remap projectile-find-file]        'counsel-projectile-find-file
    [remap projectile-find-dir]         'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             'counsel-projectile-grep
    [remap projectile-ag]               'counsel-projectile-ag
    [remap projectile-switch-project]   'counsel-projectile-switch-project)
  
  (map-local!
    :keymaps        'counsel-mode-map
    "s"             'counsel-grep-or-swiper
    "C-x C-r"       'ivy-resume)
  
  (define-key! ivy-minibuffer-map
    "TAB"           'ivy-partial-or-done
    "RET"           'ivy-alt-done))

;;
;; ido

(when (featurep! :completion ido)
  (define-key! (ido-common-completion-map ido-completion-map ido-file-completion-map)
    "\C-n"          'ido-next-match
    "\C-p"          'ido-prev-match
    "\C-w"          'ido-delete-backward-word-updir))

;;
;; window-select

(when (featurep! :ui window-select)
  (define-key!
    [remap other-window] (cond
                           ((featurep! :ui window-select +switch-window)
                                       'switch-window)
                           ((featurep! :ui window-select +ace-window)
                                       'ace-window))))
