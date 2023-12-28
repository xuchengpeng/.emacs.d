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

(defcustom dotemacs-custom-package-archives nil
  "Custom package archives."
  :type 'alist
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
  "html-head for ox-publish."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-org-html-preamble-format '(("en" ""))
  "html-preamble-format for ox-publish."
  :type 'alist
  :group 'dotemacs)

(defcustom dotemacs-org-html-postamble-format '(("en" ""))
  "html-postamble-format for ox-publish."
  :type 'alist
  :group 'dotemacs)

(provide 'dotemacs-custom)
;;; dotemacs-custom.el ends here
