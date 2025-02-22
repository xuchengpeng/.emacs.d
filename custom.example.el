;;; custom.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq user-full-name "Chuck"
      user-mail-address "xxxxxx@xxx.com")

(setq dotemacs-font (font-spec :family "IBM Plex Mono" :size 17)
      dotemacs-cn-font (font-spec :family "LXGW WenKai"))

(setq dotemacs-package-archives 'custom) ;; melpa, tuna, custom
(setq package-archives
      '(("gnu"   . "/path/to/elpa-mirror/gnu/")
        ("melpa" . "/path/to/elpa-mirror/melpa/")))

(setq dotemacs-tempel-path '("/path/to/tempel/templates"))

(provide 'custom)
;;; custom.el ends here
