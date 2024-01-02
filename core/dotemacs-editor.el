;;; dotemacs-editor.el --- Editor configurations. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Editor configuration.
;;

;;; Code:

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
      backup-directory-alist (list (cons "." (concat dotemacs-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

(setq auto-save-default nil
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat dotemacs-cache-dir "autosave/")
      tramp-auto-save-directory  (concat dotemacs-cache-dir "tramp-autosave/")
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

(setq bookmark-default-file (concat dotemacs-local-dir "bookmarks"))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil))

(use-package recentf
  :hook (after-init . recentf-mode)
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat dotemacs-cache-dir "recentf")
        recentf-max-saved-items 200
        recentf-auto-cleanup nil)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (concat dotemacs-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables
        '(kill-ring register-alist mark-ring global-mark-ring search-ring regexp-search-ring))
  (savehist-mode +1))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (concat dotemacs-cache-dir "saveplace")))

(use-package hideshow
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode))

(use-package server
  :defer 2
  :config
  (setq server-auth-dir (concat dotemacs-dir "server/"))
  (unless (server-running-p)
    (server-start)))

(use-package so-long
  :hook(after-init . global-so-long-mode))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(provide 'dotemacs-editor)
;;; dotemacs-editor.el ends here
