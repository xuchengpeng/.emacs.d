;;; core-custom.el --- Customizations. -*- lexical-binding: t; -*-

(defcustom dotemacs-full-name "Chuck"
  "Set user full name."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-mail-address "xucp@outlook.com"
  "Set user email address."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)
          (const :tag "Custom" custom))
  :group 'dotemacs)

(defcustom dotemacs-company-enable-yas nil
  "Enable/disable(t/nil) yasnippet for company backends."
  :type 'boolean
  :group 'dotemacs)

(provide 'core-custom)
;;; core-custom.el ends here
