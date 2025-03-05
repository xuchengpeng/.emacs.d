;;; custom.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "http_proxy" "http://user:password@proxy.server.com:port")
(setenv "https_proxy" "http://user:password@proxy.server.com:port")
(setenv "no_proxy" ".xxxx.com,.inner,localhost,127.0.0.1")
(setenv "GPTEL_GEMINI_KEY" "xxxxxxxxxxxxxxxxx")

(setq user-full-name "Chuck"
      user-mail-address "xxxxxx@xxx.com")

(setq dotemacs-font (font-spec :family "IBM Plex Mono" :size 17)
      dotemacs-cn-font (font-spec :family "LXGW WenKai"))

(setq dotemacs-package-archives 'custom) ;; melpa, tuna, custom
(setq package-archives
      '(("gnu"   . "/path/to/elpa-mirror/gnu/")
        ("melpa" . "/path/to/elpa-mirror/melpa/")))

(setq dotemacs-tempel-path '("/path/to/tempel/templates"))

;;; custom.el ends here
