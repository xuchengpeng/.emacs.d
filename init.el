;;; init.el --- dotemacs's configuration.

(when (version< emacs-version "28.1")
  (error "Detected Emacs %s. Emacs version should be 28.1 or higher" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define dotemacs's directory structure
(defvar dotemacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs dotemacs distribution.")
(defvar dotemacs-core-dir (expand-file-name "core/" dotemacs-dir)
  "The home of dotemacs's core functionality.")
(defvar dotemacs-modules-dir (expand-file-name  "modules/" dotemacs-dir)
  "This directory houses all of the built-in dotemacs modules.")
(defvar dotemacs-personal-dir (expand-file-name "personal/" dotemacs-dir)
  "This directory is for your personal configuration.

Users of Emacs dotemacs are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by dotemacs.")
(defvar dotemacs-personal-preload-dir (expand-file-name "preload/" dotemacs-personal-dir)
  "This directory is for your personal configuration, that you want loaded before dotemacs.")
(defvar dotemacs-modules-file (expand-file-name "dotemacs-modules.el" dotemacs-personal-dir)
  "This file contains a list of modules that will be loaded by dotemacs.")
(defvar dotemacs-local-dir (expand-file-name ".local/" dotemacs-dir)
  "Root directory for local Emacs files.")
(defvar dotemacs-cache-dir (expand-file-name ".cache/" dotemacs-dir)
  "Where cache files are stored.")

;; add dotemacs's directories to Emacs's `load-path'
(add-to-list 'load-path dotemacs-core-dir)
(add-to-list 'load-path dotemacs-modules-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" dotemacs-local-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; preload the personal settings from `dotemacs-personal-preload-dir'
(when (file-exists-p dotemacs-personal-preload-dir)
  (message "[dotemacs] Loading personal configuration files in %s..." dotemacs-personal-preload-dir)
  (mapc 'load (directory-files dotemacs-personal-preload-dir 't "^[^#\.].*el$")))

;; load core
(require 'dotemacs-core)
(require 'dotemacs-packages)
(require 'dotemacs-ui)
(require 'dotemacs-editor)
(require 'dotemacs-keybinds)

;; the modules
(if (file-exists-p dotemacs-modules-file)
    (load dotemacs-modules-file)
  (message "[dotemacs] Missing personal modules file %s" dotemacs-modules-file))

;; load the personal settings
(when (file-exists-p dotemacs-personal-dir)
  (message "[dotemacs] Loading personal configuration files in %s..." dotemacs-personal-dir)
  (mapc 'load (delete
               dotemacs-modules-file
               (directory-files dotemacs-personal-dir 't "^[^#\.].*\\.el$"))))

;;; init.el ends here