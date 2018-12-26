;;; feature/keybinds/+emacs-bindings.el -*- lexical-binding: t; -*-

(define-key!
  "M-x"             #'execute-extended-command
  "M-y"             #'yank-pop
  "C-x C-f"         #'find-file
  "C-x r b"         #'bookmark-jump
  
  "M-o" (cond
          ((featurep! :ui window-select +switch-window)
                    #'switch-window)
          ((or (featurep! :ui window-select +ace-window) t)
                    #'ace-window))
  
  "<f8>" (cond
           ((featurep! :ui neotree)
                    #'neotree-toggle)
           ((or (featurep! :ui treemacs) t)
                    #'treemacs))
  
  "C-s"             #'isearch-forward-regexp
  "C-r"             #'isearch-backward-regexp
  "C-M-s"           #'isearch-forward
  "C-M-r"           #'isearch-backward)

(define-key!
  :prefix "C-c"
  "a"               #'org-agenda
  "c"               #'org-capture
  "f"               #'hydra-flycheck/body
  "t"               #'hydra-toggle/body
  "w"               #'hydra-window/body)

(define-key! projectile-mode-map
  "C-c p"           #'projectile-command-map)

(define-key! prog-mode-map
  "C-c h"           #'hs-toggle-hiding)

(define-key! eshell-mode-map
  "C-c s"           #'+eshell/search-history)

;;
;; company

(when (featurep! :completion company)
  (define-key!
    "M-/"           #'company-complete
    "C-c C-y"       #'company-yasnippet)

  (define-key! company-active-map
    "C-p"           #'company-select-previous
    "C-n"           #'company-select-next
    "TAB"           #'company-complete-common-or-cycle
    "<tab>"         #'company-complete-common-or-cycle
    "S-TAB"         #'company-select-previous
    "<backtab>"     #'company-select-previous)
  
  (define-key! company-search-map
    "C-p"           #'company-select-previous
    "C-n"           #'company-select-next))

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
    [remap yank-pop]                  #'helm-show-kill-ring)
  
  (define-key!
    "C-x b"         #'helm-mini
    "C-x C-b"       #'helm-buffers-list
    "M-i"           #'helm-swoop
    "M-I"           #'helm-swoop-back-to-last-point
    "C-c M-i"       #'helm-multi-swoop
    "C-x M-i"       #'helm-multi-swoop-all)
  
  (define-key! helm-map
    "TAB"           #'helm-execute-persistent-action
    "<tab>"         #'helm-execute-persistent-action
    "C-z"           #'helm-select-action)
  
  (define-key! isearch-mode-map
    "M-i"           #'helm-swoop-from-isearch)
  
  (define-key! helm-swoop-map
    "M-i"           #'helm-multi-swoop-all-from-helm-swoop
    "M-m"           #'helm-multi-swoop-current-mode-from-helm-swoop
    "C-r"           #'helm-previous-line
    "C-s"           #'helm-next-line)
  
  (define-key! helm-multi-swoop-map
    "C-r"           #'helm-previous-line
    "C-s"           #'helm-next-line))

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
    [remap yank-pop]                 #'counsel-yank-pop)
  
  (define-key!
    "C-x b"         #'ivy-switch-buffer
    "C-x B"         #'ivy-switch-buffer-other-window
    "C-c C-r"       #'ivy-resume
    "C-s"           #'swiper
    "C-r"           #'swiper
    "M-i"           #'counsel-grep-or-swiper
    "C-h f"         #'counsel-describe-function
    "C-h v"         #'counsel-describe-variable)
  
  (define-key! ivy-minibuffer-map
    "TAB"           #'ivy-partial-or-done
    "RET"           #'ivy-alt-done))

;;
;; ido

(when (featurep! :completion ido)
  (define-key! (ido-common-completion-map ido-completion-map ido-file-completion-map)
    "\C-n"          #'ido-next-match
    "\C-p"          #'ido-prev-match
    "\C-w"          #'ido-delete-backward-word-updir))

;;
;; expand-region, multiple-cursors, avy

(when (featurep! :completion editor)
  (define-key!
    "C-="           #'er/expand-region
    "C-S-c C-S-c"   #'mc/edit-lines
    "C->"           #'mc/mark-next-like-this
    "C-<"           #'mc/mark-previous-like-this
    "C-c C-<"       #'mc/mark-all-like-this
    "C-:"           #'avy-goto-char
    "C-'"           #'avy-goto-char-2
    "M-g f"         #'avy-goto-line
    "M-g w"         #'avy-goto-word-1
    "M-g e"         #'avy-goto-word-0))

;;
;; magit

(when (featurep! :tools magit)
  (define-key!
    "C-x g"         #'magit-status
    "C-x M-g"       #'magit-dispatch-popup))

