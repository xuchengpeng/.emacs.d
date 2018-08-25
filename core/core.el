;;; core.el --- Initialize core configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/.emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Core configurations.
;;

;;; Code:

(when (version< emacs-version "25.2")
  (error "Emacs version should be 25.2 or higher"))

(defvar dotemacs-dir (file-truename user-emacs-directory)
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

(defvar dotemacs-init-hook ()
  "A list of hooks run when Emacs is initialized, before `dotemacs-post-init-hook'.")

(defvar dotemacs-post-init-hook ()
  "A list of hooks that run after Emacs initialization is complete, and after `dotemacs-init-hook'.")

(defvar dotemacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all dotemacs functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defgroup dotemacs nil
  "dotemacs, an Emacs configuration."
  :group 'emacs)

;;
;; Custom error types
;;

(define-error 'dotemacs-error "Error in dotemacs Emacs core")
(define-error 'dotemacs-hook-error "Error in a dotemacs startup hook" 'dotemacs-error)
(define-error 'dotemacs-autoload-error "Error in an autoloads file" 'dotemacs-error)
(define-error 'dotemacs-module-error "Error in a dotemacs module" 'dotemacs-error)
(define-error 'dotemacs-private-error "Error in private config" 'dotemacs-error)
(define-error 'dotemacs-package-error "Error with packages" 'dotemacs-error)

;;
;; Emacs core configuration
;;

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8
      system-time-locale "C")          ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 auto-mode-case-fold nil
 autoload-compute-prefixes nil
 debug-on-error dotemacs-debug-mode
 idle-update-delay 2 ;; update ui less often
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; byte compilation
 byte-compile-dynamic nil
 byte-compile-verbose dotemacs-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-scratch-message ""
 initial-major-mode 'fundamental-mode
 ;; files
 custom-file (concat dotemacs-local-dir "custom.el"))

(add-to-list 'load-path dotemacs-core-dir)

(load custom-file t (not dotemacs-debug-mode))

;;
;; Bootstrap functions
;;

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
  `dotemacs-init-hook'
  Module config.el files
  `dotemacs-post-init-hook'
  `after-init-hook'
  `emacs-startup-hook'

Module load order is determined by your `dotemacs!' block."
  ;; Make sure all essential local directories exist
  (dolist (dir (list dotemacs-local-dir dotemacs-cache-dir dotemacs-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))

  (require 'core-custom)
  (require 'core-lib)
  (require 'core-packages)
  (dotemacs-initialize-core)
  (require 'core-modules)
  (require 'core-ui)
  (require 'core-editor))

;;
;; Bootstrap dotemacs
;;

(dotemacs-initialize)

(provide 'core)
;;; core.el ends here
