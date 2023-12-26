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

(provide 'dotemacs-custom)
;;; dotemacs-custom.el ends here
