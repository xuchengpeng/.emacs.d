;;; init.el --- dotemacs's configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "30.1")
  (error "Detected Emacs %s. Emacs version should be 30.1 or higher" emacs-version))

(defvar dotemacs-dir user-emacs-directory
  "The root dir of the Emacs dotemacs distribution.")
(defvar dotemacs-local-dir (expand-file-name ".local" dotemacs-dir)
  "Root directory for local Emacs files.")
(defvar dotemacs-cache-dir (expand-file-name ".cache" dotemacs-dir)
  "Where cache files are stored.")

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir dotemacs-dir) load-path))
(let ((default-directory (expand-file-name "site-lisp" dotemacs-dir)))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-custom)
(require 'init-packages)
(require 'init-base)
(require 'init-ui)
(require 'init-editing)
(require 'init-completion)
(require 'init-org)
(require 'init-tools)
(require 'init-programming)
(require 'init-lang)

(provide 'init)
;;; init.el ends here
