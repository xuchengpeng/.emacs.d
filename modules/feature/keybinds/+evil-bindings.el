;;; feature/keybinds/+emacs-bindings.el -*- lexical-binding: t; -*-

(map!
  "b"       '(:ignore t :which-key "buffer")
  "f"       '(:ignore t :which-key "file")
  "g"       '(:ignore t :which-key "git")
  "i"       '(:ignore t :which-key "insert")
  "p"       '(:ignore t :which-key "project")
  "q"       '(:ignore t :which-key "quit")
  
  ;;buffer
  "bb"      #'switch-to-buffer
  "bk"      #'kill-this-buffer
  "bn"      #'next-buffer
  "bo"      #'dotemacs/kill-other-buffers
  "bp"      #'previous-buffer
  "bs"      #'save-buffer
  
  ;; file
  "fa"      #'projectile-find-other-file
  "fd"      #'dired
  "ff"      #'find-file
  "fr"      #'recentf-open-files
  "fs"      #'save-buffer
  "fp"      #'projectile-find-file
  
  ;;insert
  "iy"      #'yank-pop
  "is"      #'yas-insert-snippet
  
  ;;project
  "pc"      #'projectile-compile-project
  "pf"      #'projectile-find-file
  "pi"      #'projectile-invalidate-cache
  "po"      #'projectile-find-other-file
  "pp"      #'projectile-switch-project
  "pr"      #'projectile-recentf
  
  ;;quit
  "qq"      #'evil-quit-all
  "qQ"      #'evil-save-and-quit
  "qR"      #'restart-emacs)

(define-key!
  "M-x"             #'execute-extended-command
  "M-y"             #'yank-pop
  "C-x C-f"         #'find-file)

;;
;; helm

(when (featurep! :completion helm)
  (define-key!
    [remap apropos]                   #'helm-apropos
    [remap find-library]              #'helm-locate-library
    [remap bookmark-jump]             #'helm-bookmarks
    [remap execute-extended-command]  #'helm-M-x
    [remap find-file]                 #'helm-find-files
    [remap imenu-anywhere]            #'helm-imenu-anywhere
    [remap imenu]                     #'helm-semantic-or-imenu
    [remap noop-show-kill-ring]       #'helm-show-kill-ring
    [remap persp-switch-to-buffer]    #'+helm/workspace-mini
    [remap switch-to-buffer]          #'helm-buffers-list
    [remap projectile-find-file]      #'+helm/projectile-find-file
    [remap projectile-recentf]        #'helm-projectile-recentf
    [remap projectile-switch-project] #'helm-projectile-switch-project
    [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
    [remap recentf-open-files]        #'helm-recentf
    [remap yank-pop]                  #'helm-show-kill-ring))

;;
;; ivy

(when (featurep! :completion ivy)
  (define-key!
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap imenu]                    #'counsel-imenu
    [remap recentf-open-files]       #'counsel-recentf
    [remap org-capture]              #'counsel-org-capture
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap evil-ex-registers]        #'counsel-evil-registers
    [remap yank-pop]                 #'counsel-yank-pop))
