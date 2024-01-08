;;; dotemacs-core.el --- Emacs Core functions. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq idle-update-delay 1.0)  ; default is 0.5
(setq auto-mode-case-fold nil)
(setq inhibit-compacting-font-caches t)
(setq read-process-output-max (* 64 1024))  ; 64kb
(setq ffap-machine-p-known 'reject)
(setq redisplay-skip-fontification-on-input t)

(setq-default mode-line-format nil)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq system-time-locale "C")
(when IS-WINDOWS
  (setq selection-coding-system 'utf-8))
(setq-default buffer-file-coding-system 'utf-8-unix)

(provide 'dotemacs-core)
;;; dotemacs-core.el ends here
