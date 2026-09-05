;;; init.el --- dotemacs's configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "31.1")
  (error "Detected Emacs %s. Emacs version should be 31.1 or higher" emacs-version))

(defvar dotemacs-dir user-emacs-directory
  "The root dir of the Emacs dotemacs distribution.")
(defvar dotemacs-local-dir (expand-file-name ".local" dotemacs-dir)
  "Root directory for local Emacs files.")
(defvar dotemacs-cache-dir (expand-file-name ".cache" dotemacs-dir)
  "Where cache files are stored.")

(push (expand-file-name "lisp" dotemacs-dir) load-path)

(setq custom-file (expand-file-name "custom.el" dotemacs-local-dir))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(require 'init-packages)
(require 'init-base)
(require 'init-ui)
(require 'init-editor)
(require 'init-completion)
(require 'init-org)
(require 'init-tools)
(require 'init-programming)

(provide 'init)
;;; init.el ends here
