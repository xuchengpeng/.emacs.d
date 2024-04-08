;;; init-editing.el --- Editing utils. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(avy expreg multiple-cursors move-text vundo))

(setq find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t)

(setq create-lockfiles nil
      make-backup-files nil
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (expand-file-name "backup" dotemacs-cache-dir)))
      tramp-backup-directory-alist backup-directory-alist)

(setq auto-save-default nil
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (expand-file-name "autosave" dotemacs-cache-dir)
      tramp-auto-save-directory  (expand-file-name "tramp-autosave" dotemacs-cache-dir)
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Indentation
(setq-default tab-width 4
              indent-tabs-mode nil
              fill-column 120)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")

(setq kill-do-not-save-duplicates t)

(add-hook 'text-mode-hook #'visual-line-mode)

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)))

(setq bookmark-default-file (expand-file-name "bookmarks" dotemacs-cache-dir)
      project-list-file (expand-file-name "projects" dotemacs-cache-dir))

(with-eval-after-load 'multisession
  (setq multisession-directory (expand-file-name "multisession" dotemacs-cache-dir)))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :commands recentf-open-files
  :custom
  (recentf-save-file (expand-file-name "recentf" dotemacs-cache-dir))
  (recentf-max-saved-items 200))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000
        savehist-file (expand-file-name "savehist" dotemacs-cache-dir)
        savehist-save-minibuffer-history t
        savehist-autosave-interval 300
        savehist-additional-variables
        '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history)))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (expand-file-name "saveplace" dotemacs-cache-dir)))

(use-package hideshow
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode))

(use-package server
  :defer 2
  :config
  (setq server-auth-dir (expand-file-name "server" dotemacs-dir))
  (unless (server-running-p)
    (server-start)))

(use-package so-long
  :hook(after-init . global-so-long-mode))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(defun dotemacs-enable-trailing-whitespace ()
  "Delete trailing whitespace before save."
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook 'dotemacs-enable-trailing-whitespace)

(use-package avy
  :commands (avy-goto-char avy-goto-line avy-goto-word-1)
  :init
  (keymap-global-set "M-g c" 'avy-goto-char)
  (keymap-global-set "M-g g" 'avy-goto-line)
  (keymap-global-set "M-g w" 'avy-goto-word-1)
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t))

(use-package expreg
  :commands (expreg-expand)
  :init
  (keymap-global-set "C-=" 'expreg-expand))

(use-package multiple-cursors
  :commands (mc/mark-all-like-this mc/mark-next-like-this mc/mark-previous-like-this)
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" dotemacs-cache-dir))
  (keymap-global-set "C->" 'mc/mark-next-like-this)
  (keymap-global-set "C-<" 'mc/mark-previous-like-this)
  (keymap-global-set "C-c C-<" 'mc/mark-all-like-this))

(use-package move-text
  :commands (move-text-up move-text-down)
  :init
  (keymap-global-set "M-<up>" 'move-text-up)
  (keymap-global-set "M-<down>" 'move-text-down))

(use-package vundo
  :commands vundo
  :init
  (keymap-global-set "C-x u" 'vundo))

(provide 'init-editing)
;;; init-editing.el ends here
