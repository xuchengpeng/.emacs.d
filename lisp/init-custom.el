;;; init-custom.el --- Custom file  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup dotemacs nil
  "Emacs framework."
  :group 'convenience)

(defcustom dotemacs-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Tuna" tuna)
          (const :tag "Custom" custom))
  :group 'dotemacs)

(defcustom dotemacs-font nil
  "The default font to use.

Expects either a `font-spec', or a font string.

Examples:
  (setq dotemacs-font (font-spec :family \"Fira Mono\" :size 12))
  (setq dotemacs-font \"Terminus (TTF):pixelsize=12:antialias=off\")
  (setq dotemacs-font \"Iosevka SS14-13\")"
  :type 'sexp
  :group 'dotemacs)

(defcustom dotemacs-cn-font nil
  "The chinese font to use."
  :type 'sexp
  :group 'dotemacs)

(defcustom dotemacs-org-dir "~/org/"
  "Org directory."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-org-blog-dir "~/org-blog/"
  "Org blog directory."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-note-dir "~/note/"
  "Note directory."
  :type 'string
  :group 'dotemacs)

(setq custom-file (expand-file-name "custom.el" dotemacs-local-dir))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init-custom)
;;; init-custom.el ends here
