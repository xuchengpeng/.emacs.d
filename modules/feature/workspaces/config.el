;;; feature/workspaces/config.el -*- lexical-binding: t; -*-

(defvar +workspaces-data-file "_workspaces"
  "The basename of the file to store single workspace perspectives. Will be
stored in `persp-save-dir'.")

(use-package persp-mode
  :hook (after-init . persp-mode)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-auto-save-opt (if noninteractive 0 1) ; auto-save on kill
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-save-dir (concat dotemacs-local-dir "workspaces/"))
  
  (advice-add #'persp-asave-on-exit :around #'+workspaces*autosave-real-buffers)
  
  (add-hook 'dotemacs-cleanup-hook #'+workspaces|cleanup-unassociated-buffers)
  
  ;;
  ;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))
  ;; compile
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars
   '(major-mode default-directory compilation-directory compilation-environment compilation-arguments)))

