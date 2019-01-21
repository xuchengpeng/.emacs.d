;;; completion/ivy/config.el -*- lexical-binding: t -*-

(use-package ivy
  :diminish ivy-mode
  :defer t
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'full
        ivy-magic-tilde nil
        ;;ivy-dynamic-exhibit-delay-ms 150
        ivy-count-format "(%d/%d) "
        ivy-format-function #'ivy-format-function-line)
  ;; Integration with `magit'
  (after-load! 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  (ivy-mode +1))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (dolist (cmd '(counsel-projectile-switch-to-buffer))
    (ivy-set-display-transformer cmd 'ivy-rich--ivy-switch-buffer-transformer)))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package flx
  :when (featurep! +fuzzy)
  :defer t  ; is loaded by ivy
  :config
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package swiper
  :diminish
  :defer t)

(use-package counsel
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))
  (let ((command
         (cond
          ((executable-find "rg")
           "rg --smart-case --no-heading --line-number --color never %s %s")
          ((executable-find "ag")
           "ag --smart-case --noheading --nocolor --numbers %s %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))
  (setq counsel-rg-base-command "rg --smart-case --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag --smart-case --nocolor --nogroup --numbers %s")
  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")))

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

(use-package amx
  :defer t
  :config
  (setq amx-history-length 10
        amx-save-file (concat dotemacs-cache-dir "amx-items")))
