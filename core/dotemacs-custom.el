;;; dotemacs-custom.el --- Custom. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  (setq dotemacs-font \"Terminus (TTF):pixelsize=12:antialias=off\")"
  :type 'sexp
  :group 'dotemacs)

(defcustom dotemacs-cn-font nil
  "The chinese font to use."
  :type 'sexp
  :group 'dotemacs)

(defcustom dotemacs-tempel-path nil
  "A list of template files."
  :type '(repeat string)
  :group 'dotemacs)

(defcustom dotemacs-org-dir "~/org/"
  "Org directory."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-org-site-dir "~/org-site/"
  "Org site directory."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-org-html-head ""
  "Org html-head for ox-publish."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-org-html-preamble-format '(("en" ""))
  "Org html-preamble-format for ox-publish."
  :type 'alist
  :group 'dotemacs)

(defcustom dotemacs-org-html-postamble-format '(("en" ""))
  "Org html-postamble-format for ox-publish."
  :type 'alist
  :group 'dotemacs)

(provide 'dotemacs-custom)
;;; dotemacs-custom.el ends here
