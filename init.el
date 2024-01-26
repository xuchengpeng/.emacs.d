;;; init.el --- dotemacs's configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "29.1")
  (error "Detected Emacs %s. Emacs version should be 29.1 or higher" emacs-version))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(defgroup dotemacs nil
  "Emacs framework."
  :group 'convenience)

(defvar dotemacs-dir user-emacs-directory
  "The root dir of the Emacs dotemacs distribution.")
(defvar dotemacs-local-dir (expand-file-name ".local/" dotemacs-dir)
  "Root directory for local Emacs files.")
(defvar dotemacs-cache-dir (expand-file-name ".cache/" dotemacs-dir)
  "Where cache files are stored.")

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir dotemacs-dir) load-path))
(let ((default-directory (expand-file-name "site-lisp" dotemacs-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (expand-file-name "custom.el" dotemacs-local-dir))
(when (file-exists-p custom-file)
  (load custom-file 'nomessage t))

(require 'init-benchmark)
(require 'init-custom)
(require 'init-utils)
(require 'init-packages)
(require 'init-keybinds)
(require 'init-ui)
(require 'init-editing)
(require 'init-dired)
(require 'init-dashboard)
(require 'init-completion)
(require 'init-modeline)
(require 'init-highlight)
(require 'init-programming)
(require 'init-eshell)
(require 'init-org)
(require 'init-lang)

;;; init.el ends here
