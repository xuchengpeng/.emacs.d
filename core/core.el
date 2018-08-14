;;; core.el --- Initialize core configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/emacs.d

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

(defvar dotemacs-autoload-file (concat dotemacs-local-dir "autoload.el")
  "The path of autoload file which has all the autoload functions.")

(defvar dotemacs-init-hook ()
  "A list of hooks run when Emacs is initialized, before `dotemacs-post-init-hook'.")

(defvar dotemacs-post-init-hook ()
  "A list of hooks that run after Emacs initialization is complete, and after `dotemacs-init-hook'.")

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq garbage-collection-messages t)
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)
(setq custom-file (concat dotemacs-local-dir "custom.el"))
(load custom-file t t)

(add-to-list 'load-path dotemacs-core-dir)

;; preload the personal settings from `dotemacs-personal-preload-dir'
(let ((file-list (directory-files dotemacs-personal-preload-dir t "^[^#\.].*el$")))
  (when file-list
    (message "Loading personal configuration files in %s..." dotemacs-personal-preload-dir)
    (dolist (file file-list)
     (load file t t))))

(defun dotemacs-initialize ()
  "dotemacs initialize function.
The load order is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  `after-init-hook'
  `emacs-startup-hook'
  Module packages.el files
  Module config.el files
  dotemacs-init-hook
  dotemacs-post-init-hook

Module load order is determined by your `dotemacs!' block."
  (require 'core-custom)
  (require 'core-lib)
  (require 'core-packages)
  
  (dotemacs-ensure-packages-initialized)
  (dotemacs-ensure-core-packages)
  
  (require 'core-ui)
  (require 'core-editor))

(defun dotemacs-finalize ()
  "dotemacs finalize function."
  (dotemacs-load-autoload)
  
  (dotemacs-initialize-modules)
  
  (unless noninteractive
    (dolist (hook '(dotemacs-init-hook dotemacs-post-init-hook))
      (run-hook-with-args hook)))
  
  (when (display-graphic-p)
    (require 'server)
    (unless (server-running-p)
      (server-start)))
  
  (setq file-name-handler-alist file-name-handler-alist-old
        gc-cons-threshold 800000
        gc-cons-percentage 0.1))

(dotemacs-initialize)
(add-hook 'emacs-startup-hook #'dotemacs-finalize t)

;; load the personal settings from `dotemacs-personal-dir`
(let ((file-list (directory-files dotemacs-personal-dir t "^[^#\.].*el$")))
  (when file-list
    (message "Loading personal configuration files in %s..." dotemacs-personal-dir)
    (dolist (file file-list)
     (load file t t))))

(provide 'core)

;;; core.el ends here
