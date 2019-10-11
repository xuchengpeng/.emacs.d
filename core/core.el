;;; core.el --- Initialize core configurations. -*- lexical-binding: t; -*-

(when (version< emacs-version "25.2")
  (error "Emacs version should be 25.2 or higher"))

;; Ensure `dotemacs-core-dir' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

;;
;;; Global variables

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defvar dotemacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs.d directory.")

(defvar dotemacs-core-dir (concat dotemacs-dir "core/")
  "Where core files are stored.")

(defvar dotemacs-modules-dir (concat dotemacs-dir "modules/")
  "Where modules files are stored.")

(defvar dotemacs-local-dir (concat dotemacs-dir ".local/")
  "Root directory for local Emacs files.")

(defvar dotemacs-cache-dir (concat dotemacs-dir ".cache/")
  "Where cache files are stored.")

(defvar dotemacs-packages-dir (concat dotemacs-local-dir "packages/")
  "Where packages are stored.")

(defvar dotemacs-private-dir nil
  "Where your private customizations are placed. Must end in a slash.")

(defvar dotemacs-autoload-file (concat dotemacs-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")

(defvar dotemacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all dotemacs functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defvar dotemacs-interactive-mode (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defgroup dotemacs nil
  "dotemacs, an Emacs configuration."
  :group 'emacs)

;;
;;; Startup optimizations

(setq gc-cons-threshold most-positive-fixnum)

(defvar dotemacs-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")
(defvar dotemacs--initial-file-name-handler-alist file-name-handler-alist)

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(setq file-name-handler-alist nil)

(defun dotemacs-restore-file-name-handler-alist-h ()
  (setq file-name-handler-alist dotemacs--initial-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'dotemacs-restore-file-name-handler-alist-h)

;; To speed up minibuffer commands (like helm and ivy), we defer garbage
;; collection while the minibuffer is active.
(defun dotemacs-defer-garbage-collection-h ()
  "TODO"
  (setq gc-cons-threshold most-positive-fixnum))

(defun dotemacs-restore-garbage-collection-h ()
  "TODO"
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold dotemacs-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'dotemacs-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'dotemacs-restore-garbage-collection-h)

;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook 'emacs-startup-hook #'dotemacs-restore-garbage-collection-h)

;; When Emacs loses focus seems like a great time to do some garbage collection
;; all sneaky breeky like, so we can return a fresh(er) Emacs.
(add-hook 'focus-out-hook #'garbage-collect)

;;
;;; Custom error types

(define-error 'dotemacs-error "Error in dotemacs Emacs core")
(define-error 'dotemacs-hook-error "Error in a dotemacs startup hook" 'dotemacs-error)
(define-error 'dotemacs-autoload-error "Error in an autoloads file" 'dotemacs-error)
(define-error 'dotemacs-module-error "Error in a dotemacs module" 'dotemacs-error)
(define-error 'dotemacs-private-error "Error in private config" 'dotemacs-error)
(define-error 'dotemacs-package-error "Error with packages" 'dotemacs-error)

;;
;;; Emacs core configuration

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error dotemacs-debug-mode
      jka-compr-verbose dotemacs-debug-mode)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system 'utf-8)          ; pretty
(setq selection-coding-system 'utf-8)  ; pretty
(setq locale-coding-system 'utf-8)     ; please
(if IS-WINDOWS (set-w32-system-coding-system 'utf-8)) ; with sugar on top

;; Disable warnings from legacy advice system. They aren't useful, and we can't
;; often do anything about them besides changing packages upstream
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

;; Don't make a second case-insensitive pass over `auto-mode-alist'. If it has
;; to, it's our (the user's) failure. One case for all!
(setq auto-mode-case-fold nil)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

(setq abbrev-file-name             (concat dotemacs-local-dir "abbrev.el")
      async-byte-compile-log-file  (concat dotemacs-cache-dir "async-bytecomp.log")
      bookmark-default-file        (concat dotemacs-cache-dir "bookmarks")
      pcache-directory             (concat dotemacs-cache-dir "pcache/")
      request-storage-directory    (concat dotemacs-cache-dir "request")
      shared-game-score-directory  (concat dotemacs-cache-dir "shared-game-score/")
      tramp-auto-save-directory    (concat dotemacs-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat dotemacs-cache-dir "tramp-persistency.el")
      url-cache-directory          (concat dotemacs-cache-dir "url/")
      url-configuration-directory  (concat dotemacs-cache-dir "url/"))

(unless custom-file
  (setq custom-file (concat dotemacs-local-dir "custom.el")))
(load custom-file t (not dotemacs-debug-mode))

;;
;;; Bootstrap helpers

(defun dotemacs-try-run-hook (hook)
  "Run HOOK (a hook function), but marks thrown errors to make it a little
easier to tell where the came from.

Meant to be used with `run-hook-wrapped'."
  (when dotemacs-debug-mode
    (message "Running dotemacs hook: %s" hook))
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'dotemacs-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

;; benchmark
(defun dotemacs-display-benchmark-h (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Emacs loaded %s packages across %d modules in %.03fs"
           (+ (length dotemacs-core-packages) (length dotemacs-packages))
           (if dotemacs-modules (hash-table-count dotemacs-modules) 0)
           (or dotemacs-init-time
               (setq dotemacs-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
(add-hook 'window-setup-hook #'dotemacs-display-benchmark-h)

(defun dotemacs-initialize ()
  "dotemacs initialize function.
The load order is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  Module packages.el files
  Module init.el files
  `dotemacs-before-init-modules-hook'
  Module config.el files
  `dotemacs-init-modules-hook'
  `after-init-hook'
  `emacs-startup-hook'
  `window-setup-hook'

Module load order is determined by your `dotemacs!' block."
  ;; Make sure all essential local directories exist
  (dolist (dir (list dotemacs-local-dir dotemacs-cache-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))

  (require 'core-custom)
  (require 'core-lib)
  (require 'core-packages)
  (dotemacs-initialize-packages)
  (require 'core-ui)
  (require 'core-editor)
  (require 'core-keybinds)
  (require 'core-modules))

(defun dotemacs-finalize ()
  "dotemacs finalize function.")

(add-hook 'emacs-startup-hook #'dotemacs-finalize)

;;
;;; Bootstrap dotemacs

(dotemacs-initialize)

(provide 'core)
;;; core.el ends here
