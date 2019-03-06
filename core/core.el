;;; core.el --- Initialize core configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/.emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Core configurations.
;;

;;; Code:

(when (version< emacs-version "25.2")
  (error "Emacs version should be 25.2 or higher"))

(defvar dotemacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs.d directory.")

(defvar dotemacs-core-dir (concat dotemacs-dir "core/")
  "Where core files are stored.")

(defvar dotemacs-modules-dir (concat dotemacs-dir "modules/")
  "Where modules files are stored.")

(defvar dotemacs-vendor-dir (concat dotemacs-dir "vendor/")
  "Where vendor files are stored.")

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

(defvar dotemacs-before-init-modules-hook nil
  "A list of hooks run before modules' config.el files are loaded, but after their init.el files are loaded.")

(defvar dotemacs-init-modules-hook nil
  "A list of hooks that run after modules' config.el files have loaded.")

(defvar dotemacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all dotemacs functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defgroup dotemacs nil
  "dotemacs, an Emacs configuration."
  :group 'emacs)

;;
;; Startup optimizations

(defvar dotemacs-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar dotemacs-gc-cons-upper-limit 268435456 ; 256mb
  "The temporary value for `gc-cons-threshold' to defer it.")
  
(defvar dotemacs--file-name-handler-alist file-name-handler-alist)

(unless after-init-time
  (setq gc-cons-threshold dotemacs-gc-cons-upper-limit
        file-name-handler-alist nil))

(defun dotemacs|restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist dotemacs--file-name-handler-alist)
  (run-with-idle-timer
   3 nil (lambda () (setq-default gc-cons-threshold dotemacs-gc-cons-threshold))))

(add-hook 'after-init-hook #'dotemacs|restore-startup-optimizations)

;;
;; Custom error types

(define-error 'dotemacs-error "Error in dotemacs Emacs core")
(define-error 'dotemacs-hook-error "Error in a dotemacs startup hook" 'dotemacs-error)
(define-error 'dotemacs-autoload-error "Error in an autoloads file" 'dotemacs-error)
(define-error 'dotemacs-module-error "Error in a dotemacs module" 'dotemacs-error)
(define-error 'dotemacs-private-error "Error in private config" 'dotemacs-error)
(define-error 'dotemacs-package-error "Error with packages" 'dotemacs-error)

;;
;; Constants

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;;
;; Emacs core configuration

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system 'utf-8)          ; pretty
(setq selection-coding-system 'utf-8)  ; pretty
(setq locale-coding-system 'utf-8)     ; please
(if IS-WINDOWS (set-w32-system-coding-system 'utf-8)) ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 auto-mode-case-fold nil
 autoload-compute-prefixes nil
 debug-on-error dotemacs-debug-mode
 ffap-machine-p-known 'reject     ; don't ping things that look like domain names
 idle-update-delay 2              ;; update ui less often
 ;; History & backup settings
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; byte compilation
 byte-compile-verbose dotemacs-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-scratch-message nil
 initial-major-mode 'fundamental-mode
 ;; files
 abbrev-file-name             (concat dotemacs-local-dir "abbrev.el")
 auto-save-list-file-name     (concat dotemacs-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat dotemacs-cache-dir "backup/")))
 pcache-directory             (concat dotemacs-cache-dir "pcache/")
 request-storage-directory    (concat dotemacs-cache-dir "request")
 shared-game-score-directory  (concat dotemacs-cache-dir "shared-game-score/")
 tramp-auto-save-directory    (concat dotemacs-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat dotemacs-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat dotemacs-cache-dir "url/")
 url-configuration-directory  (concat dotemacs-cache-dir "url/"))

(add-to-list 'load-path dotemacs-core-dir)

(unless custom-file
  (setq custom-file (concat dotemacs-local-dir "custom.el")))
(load custom-file t (not dotemacs-debug-mode))

;;
;; Bootstrap functions

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
  (dolist (dir (list dotemacs-local-dir dotemacs-cache-dir dotemacs-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))

  (require 'core-custom)
  (require 'core-lib)
  (require 'core-packages)
  (dotemacs-initialize-core)
  (require 'core-ui)
  (require 'core-editor)
  (require 'core-keybinds)
  (require 'core-modules))

(defun dotemacs-finalize ()
  "dotemacs finalize function.")

(add-hook 'emacs-startup-hook #'dotemacs-finalize)

;;
;; Bootstrap dotemacs

(dotemacs-initialize)

(provide 'core)
;;; core.el ends here
