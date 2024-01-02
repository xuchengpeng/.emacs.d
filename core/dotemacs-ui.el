;;; dotemacs-ui.el --- Initialize dotemacs ui configurations. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; UI configuration.
;;

;;; Code:

(defvar dotemacs-font nil
  "The default font to use.

Expects either a `font-spec', or a font string.

Examples:
  (setq dotemacs-font (font-spec :family \"Fira Mono\" :size 12))
  (setq dotemacs-font \"Terminus (TTF):pixelsize=12:antialias=off\")")

(defvar dotemacs-cn-font nil
  "The chinese font to use.")

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . 2))
      mouse-wheel-scroll-amount-horizontal 2)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

(setq window-resize-pixelwise nil
      frame-resize-pixelwise t)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'text-mode)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'text-mode
      initial-scratch-message nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package winner
  :hook (window-configuration-change . winner-mode)
  :config
  (setq winner-boring-buffers (append winner-boring-buffers
        '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
          "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
          "*esh command on file*"))))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(defun dotemacs-set-font()
  "Set english and chinese fonts."
  (when dotemacs-font
    (set-face-attribute 'default nil :font dotemacs-font))
  (when dotemacs-cn-font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset dotemacs-cn-font))))

(defun dotemacs-init-fonts (&optional _)
  "Set the font."
  (add-to-list 'after-make-frame-functions
               (lambda (new-frame)
                 (select-frame new-frame)
                 (when (display-graphic-p)
                   (dotemacs-set-font))))
  (when (display-graphic-p)
    (dotemacs-set-font)))

(add-hook 'window-setup-hook #'dotemacs-init-fonts)

(provide 'dotemacs-ui)
;;; dotemacs-ui.el ends here