
(when (version< emacs-version "25.2")
  (error "Emacs version should be 25.2 or higher"))

(defconst dotemacs-dir (file-truename user-emacs-directory)
  "The path to this emacs.d directory.")
(defconst dotemacs-core-dir (concat dotemacs-dir "core/")
  "Where core files are stored.")
(defconst dotemacs-modules-dir (concat dotemacs-dir "modules/")
  "Where modules files are stored.")
(defconst dotemacs-local-dir (concat dotemacs-dir ".local/")
  "Root directory for local Emacs files.")
(defconst dotemacs-packages-dir (concat dotemacs-local-dir "packages/")
  "Where packages are stored.")
(defconst dotemacs-personal-dir (concat dotemacs-dir "personal/")
  "This directory is for your personal configuration.
Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defconst dotemacs-personal-preload-dir (concat dotemacs-personal-dir "preload/")
  "This directory is for your personal configuration, that you want loaded before dotemacs.")
(defconst dotemacs-cache-dir (concat dotemacs-dir ".cache/")
  "Where cache files are stored.")

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq garbage-collection-messages t)
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)
(setq custom-file (concat dotemacs-local-dir "custom.el"))
(load custom-file t t t)

(add-to-list 'load-path dotemacs-core-dir)

;; preload the personal settings from `dotemacs-personal-preload-dir'
(when (file-exists-p dotemacs-personal-preload-dir)
  (message "Loading personal configuration files in %s..." dotemacs-personal-preload-dir)
  (mapc 'load (directory-files dotemacs-personal-preload-dir 't "^[^#\.].*el$")))

(require 'core-custom)
(require 'core-lib)
(require 'core-packages)

(defun dotemacs-initialize ()
  (dotemacs-ensure-packages-initialized)
  (dotemacs-ensure-core-packages)
  
  (require 'core-ui)
  (require 'core-editor))

(dotemacs-initialize)

(defun dotemacs-finalize ()
  (setq file-name-handler-alist file-name-handler-alist-old
        gc-cons-threshold 800000
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'dotemacs-finalize t)

(require 'server)
(unless (server-running-p)
  (server-start))

;; load the personal settings from `dotemacs-personal-dir`
(when (file-exists-p dotemacs-personal-dir)
  (message "Loading personal configuration files in %s..." dotemacs-personal-dir)
  (mapc 'load (directory-files dotemacs-personal-dir 't "^[^#\.].*el$")))

(provide 'core)

;;; core.el ends here
