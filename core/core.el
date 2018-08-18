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

(defvar dotemacs-personal-dir (concat dotemacs-dir "personal/")
  "This directory is for your personal configuration.
Users of dotemacs are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by dotemacs.")

(defvar dotemacs-personal-preload-dir (concat dotemacs-personal-dir "preload/")
  "This directory is for your personal configuration, that you want loaded before dotemacs.")

(defvar dotemacs-cache-dir (concat dotemacs-dir ".cache/")
  "Where cache files are stored.")

(defvar dotemacs-packages-dir (concat dotemacs-local-dir "packages/")
  "Where packages are stored.")

(defvar dotemacs-private-dir ()
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
 initial-major-mode 'text-mode
 ;; files
 custom-file (concat dotemacs-local-dir "custom.el"))

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq garbage-collection-messages t)
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)

(add-to-list 'load-path dotemacs-core-dir)

(load custom-file t (not dotemacs-debug-mode))

;;
;; Bootstrap functions
;;

(defun dotemacs-initialize ()
  "dotemacs initialize function.
The load order is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  Module packages.el files
  Module config.el files
  `after-init-hook'
  `emacs-startup-hook'
  dotemacs-init-hook
  dotemacs-post-init-hook

Module load order is determined by your `dotemacs!' block."
  ;; preload the personal settings from `dotemacs-personal-preload-dir'
  (let ((file-list
         (file-expand-wildcards
          (expand-file-name "*.el" dotemacs-personal-preload-dir))))
    (dolist (file file-list)
      (load file t (not dotemacs-debug-mode))))

  (require 'core-custom)
  (require 'core-lib)
  (require 'core-packages)
  (dotemacs-initialize-core)
  (require 'core-ui)
  (require 'core-editor))

(defun dotemacs-finalize ()
  "dotemacs finalize function."
  (unless noninteractive
    (dolist (hook '(dotemacs-init-hook dotemacs-post-init-hook))
      (run-hook-with-args hook)))
  
  (when (display-graphic-p)
    (require 'server)
    (unless (server-running-p)
      (server-start)))
  
  ;; load the personal settings from `dotemacs-personal-dir`
  (let ((file-list
         (file-expand-wildcards
          (expand-file-name "*.el" dotemacs-personal-dir))))
    (dolist (file file-list)
      (load file t (not dotemacs-debug-mode))))
  
  (setq file-name-handler-alist file-name-handler-alist-old
        gc-cons-threshold 800000
        gc-cons-percentage 0.1))

;;
;; Bootstrap dotemacs
;;

(dotemacs-initialize)
(add-hook 'emacs-startup-hook #'dotemacs-finalize t)

(provide 'core)

;;; core.el ends here
