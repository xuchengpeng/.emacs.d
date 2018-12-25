;;; feature/keybinds/+emacs-bindings.el -*- lexical-binding: t; -*-

(define-key!
  "C-s"             #'isearch-forward-regexp
  "C-r"             #'isearch-backward-regexp
  "C-M-s"           #'isearch-forward
  "C-M-r"           #'isearch-backward)

;;
;; hideshow

(define-key! prog-mode-map
  "C-c h"           #'hs-toggle-hiding)

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
    "M-x"           #'helm-M-x
    "M-y"           #'helm-show-kill-ring
    "C-x r b"       #'helm-filtered-bookmarks
    "C-x C-f"       #'helm-find-files
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
    "C-x b"         #'ivy-switch-buffer
    "C-x B"         #'ivy-switch-buffer-other-window
    "C-c C-r"       #'ivy-resume
    "C-s"           #'swiper
    "C-r"           #'swiper
    "M-x"           #'counsel-M-x
    "M-y"           #'counsel-yank-pop
    "M-i"           #'counsel-grep-or-swiper
    "C-x r b"       #'counsel-bookmark
    "C-x C-f"       #'counsel-find-file
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
;; org

(when (featurep! :lang org)
  (define-key!
    "C-c a"         #'org-agenda
    "C-c c"         #'org-capture))

;;
;; magit

(when (featurep! :tools magit)
  (define-key!
    "C-x g"         #'magit-status
    "C-x M-g"       #'magit-dispatch-popup))

;;
;; neotree

(when (featurep! :ui neotree)
  (define-key!
    "<f8>"          #'neotree-toggle))

;;
;; treemacs

(when (featurep! :ui treemacs)
  (define-key!
    "<f8>"          #'treemacs))

;;
;; window-select

(cond ((featurep! :ui window-select +switch-window)
       (define-key!
         "M-o"      #'switch-window))
      ((or (featurep! :ui window-select +ace-window) t)
       (define-key!
         "M-o"      #'ace-window)))

;;
;; hydra

(when (featurep! :tools utils)
  (define-key!
    "C-c f"         #'hydra-flycheck/body
    "C-c t"         #'hydra-toggle/body
    "C-c w"         #'hydra-window/body))
