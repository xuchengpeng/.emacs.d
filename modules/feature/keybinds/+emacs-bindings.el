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
  "/I"      '(imenu-anywhere :which-key "Jump to symbol across buffers")
  "/b"      '(swiper :which-key "Search buffer")
  "/i"      '(imenu :which-key "Jump to symbol")
  "/l"      '(ace-link :which-key "Jump to link")
  "/p"      '(projectile-ripgrep :which-key "Search project")
  
  ;; buffer
  "bS"      '(dotemacs/sudo-this-file :which-key "Sudo edit this file")
  "bb"      '(switch-to-buffer :which-key "Switch buffer")
  "bk"      '(kill-this-buffer :which-key "Kill buffer")
  "bn"      '(next-buffer :which-key "Next buffer")
  "bo"      '(dotemacs/kill-other-buffers :which-key "Kill other buffers")
  "bp"      '(previous-buffer :which-key "Previous buffer")
  "bs"      '(save-buffer :which-key "Save buffer")
  "bz"      '(bury-buffer :which-key "Bury buffer")
  
  ;; code
  "cF"      '(hydra-flycheck/body :which-key "Flycheck")
  "cf"      '(clang-format :which-key "Format")
  "cg"      '(hydra-avy/body :which-key "Goto")
  "cm"      '(hydra-multiple-cursors/body :which-key "Multiple cursors")
  
  ;; file
  "fR"      '(projectile-recentf :which-key "Recent project files")
  "fa"      '(projectile-find-other-file :which-key "Find other file")
  "fd"      '(dired :which-key "Find directory")
  "ff"      '(find-file :which-key "Find file")
  "fr"      '(recentf-open-files :which-key "Recent files")
  "fs"      '(save-buffer :which-key "Save file")
  "fp"      '(projectile-find-file :which-key "Find file in project")
  
  ;; git
  "gb"      '(magit-blame-addition :which-key "Magit blame")
  "gc"      '(magit-commit :which-key "Magit commit")
  "gd"      '(magit-dispatch-popup :which-key "Magit dispatch")
  "gf"      '(magit-find-file :which-key "Magit find file")
  "gx"      '(magit-file-delete :which-key "Magit file delete")
  "gh"      '(magithub-dispatch-popup :which-key "Magithub dispatch")
  "gi"      '(magit-init :which-key "Initialize repo")
  "gl"      '(magit-log-buffer-file :which-key "Magit buffer log")
  "gL"      '(magit-list-repositories :which-key "List repositories")
  "gs"      '(magit-status :which-key "Magit status")
  "gS"      '(magit-stage-file :which-key "Git stage file")
  "gU"      '(magit-unstage-file :which-key "Git unstage file")
  "gp"      '(magit-push-file :which-key "Magit push popup")
  "gP"      '(magit-pull-popup :which-key "Magit pull popup")
  
  ;; help
  "hF"      '(describe-face :which-key "Describe face")
  "hM"      '(describe-mode :which-key "Describe mode")
  "hc"      '(describe-char :which-key "Describe char")
  "hf"      '(describe-function :which-key "Describe function")
  "hi"      '(info-lookup-symbol :which-key "Info")
  "hk"      '(describe-key :which-key "Describe key")
  "hl"      '(find-library :which-key "Find library")
  "hv"      '(describe-variable :which-key "Describe variable")
  
  ;; insert
  "iy"      '(yank-pop :which-key "Insert from clipboard")
  "is"      '(yas-insert-snippet :which-key "Insert snippet")
  
  ;; open
  "oa"      '(org-agenda :which-key "Org agenda")
  "ob"      '(browse-url-of-file :which-key "Default browser")
  "oc"      '(org-capture :which-key "Org capture")
  "od"      '(dired-jump :which-key "Dired")
  "ow"      '(hydra-window/body :which-key "Window")
  
  ;; project
  "pc"      '(projectile-compile-project :which-key "Compile project")
  "pf"      '(projectile-find-file :which-key "Find file in project")
  "pi"      '(projectile-invalidate-cache :which-key "Invalidate cache")
  "po"      '(projectile-find-other-file :which-key "Find other file")
  "pp"      '(projectile-switch-project :which-key "Switch project")
  "pr"      '(projectile-recentf :which-key "Recent project files")
  "ps"      '(:ignore t :which-key "Project search")
  "psg"     '(projectile-grep :which-key "grep")
  "psa"     '(projectile-ag :which-key "ag")
  "psr"     '(projectile-ripgrep :which-key "ripgrep")
  
  ;; quit
  "qq"      '(evil-quit-all :which-key "Quit Emacs")
  "qQ"      '(evil-save-and-quit :which-key "Save and quit Emacs")
  "qR"      '(restart-emacs :which-key "Restart Emacs")
  
  ;; snippets
  "s/"      '(yas-visit-snippet-file :which-key "Jump to mode snippet")
  "sn"      '(yas-new-snippet :which-key "New snippet")
  "si"      '(yas-insert-snippet :which-key "Insert snippet")
  "sr"      '(yas-reload-all :which-key "Reload snippets")
  
  ;; toggle
  "ta"      '(aggressive-indent-mode :which-key "Aggressive indent")
  "tf"      '(flycheck-mode :which-key "Flycheck")
  "tF"      '(toggle-frame-fullscreen :which-key "Frame fullscreen")
  "ti"      '(highlight-indentation-mode :which-key "Indent guides")
  "tI"      '(highlight-indentation-current-column-mode :which-key "Indent guides (column)")
  "tl"      '(display-line-numbers-mode :which-key "Line numbers")
  "to"      '(org-mode :which-key "Org")
  "ts"      '(smartparens-mode :which-key "Smartparens"))

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
