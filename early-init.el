;;; early-init.el --- Early initialization  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

(let ((old-value file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist old-value))))
            101))

(setq native-comp-jit-compilation nil)
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-compacting-font-caches t
      use-file-dialog nil
      use-dialog-box nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs")
      icon-title-format frame-title-format)

(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq-default mode-line-format nil)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq auto-mode-case-fold nil
      read-process-output-max (* 4 1024 1024))

(provide 'early-init)
;;; early-init.el ends here
