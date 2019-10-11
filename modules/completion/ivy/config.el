;;; completion/ivy/config.el -*- lexical-binding: t -*-

(use-package ivy
  :diminish ivy-mode
  :defer t
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          ;; Ignore order for non-fuzzy searches by default
          (t . ivy--regex-ignore-order)))
  :config
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

(when (and (featurep! +fuzzy)
           (not (featurep! +prescient)))
  (use-package flx
    :defer t  ; is loaded by ivy
    :init
    (setf (alist-get 't ivy-re-builders-alist) #'ivy--regex-fuzzy)
    (setq ivy-initial-inputs-alist nil
          ivy-flx-limit 10000)))

(when (featurep! +prescient)
  (use-package ivy-prescient
    :hook (ivy-mode . ivy-prescient-mode)
    :init
    (setq prescient-filter-method
          (if (featurep! +fuzzy)
              '(literal regexp initialism fuzzy)
            '(literal regexp initialism))
          ivy-prescient-enable-filtering nil  ; we do this ourselves
          ivy-prescient-retain-classic-highlighting t
          ivy-initial-inputs-alist nil
          ivy-re-builders-alist
          '((counsel-ag . +ivy-prescient-non-fuzzy)
            (counsel-rg . +ivy-prescient-non-fuzzy)
            (counsel-grep . +ivy-prescient-non-fuzzy)
            (swiper . +ivy-prescient-non-fuzzy)
            (swiper-isearch . +ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder)))
    :config
    (defun +ivy-prescient-non-fuzzy (str)
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))
    
    ;; NOTE prescient config duplicated with `company'
    (setq prescient-save-file (concat dotemacs-cache-dir "prescient-save.el"))
    (prescient-persist-mode +1)))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (dolist (cmd '(ivy-switch-buffer-other-window
                 counsel-projectile-switch-to-buffer))
    (ivy-set-display-transformer cmd 'ivy-rich--ivy-switch-buffer-transformer)))

(use-package ivy-hydra
  :after (ivy hydra))

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
         "\\|\\(?:[#~]\\'\\)")))

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
