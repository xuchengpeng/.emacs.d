;;; core-ui.el --- Initialize core ui configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/.emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Core ui configurations.
;;

;;; Code:

(defvar dotemacs-theme nil
  "A symbol representing the color theme to load.")

(defvar dotemacs-load-theme-hook nil
  "Hook run when the theme is initialized.")

(defvar dotemacs-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defvar dotemacs-font nil
  "The default font to use.")

(defvar dotemacs-font-size nil
  "The default font size to use.")

(defvar dotemacs-cn-font nil
  "The default chinese font to use.")

(defvar dotemacs-cn-font-size nil
  "The default chinese font size to use.")

(setq-default
 blink-matching-paren nil         ; don't blink--too distracting
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-scroll-output 'first-error
 confirm-nonexistent-file-or-buffer t
 cursor-in-non-selected-windows nil ; hide cursors in other windows
 display-line-numbers-width 3
 enable-recursive-minibuffers nil
 frame-inhibit-implied-resize t
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))
 highlight-nonselected-windows nil
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 inhibit-compacting-font-caches t
 mode-line-default-help-echo nil ; disable mode-line mouseovers
 mouse-yank-at-point t           ; middle-click paste at point, not at click
 show-help-function nil          ; hide :help-echo text
 uniquify-buffer-name-style 'forward
 uniquify-separator "/"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 use-dialog-box nil              ; always avoid GUI
 user-full-name    dotemacs-full-name
 user-mail-address dotemacs-mail-address
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil
 ;; don't resize emacs in steps, it looks weird
 window-resize-pixelwise t
 frame-resize-pixelwise t)

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; removes the GUI elements
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(unless IS-MAC
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))
;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

;; (line-number-mode t)
;; (column-number-mode t)
;; (size-indication-mode t)

;;
;; Built-in packages

;; undo/redo changes to Emacs' window layout
(use-package winner
  :hook (window-configuration-change . winner-mode))

;; Highlight the current line
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode))

;; highlight matching delimiters
(use-package paren
  :hook (find-file . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

;;
;; Theme & font

(defun dotemacs|set-font()
  "Set english and chinese fonts."
  (setq fonts
        (cond (IS-MAC
               '("Monaco" "STHeiti"))
              (IS-LINUX
               '("DejaVu Sans Mono" "STHeiti"))
              (IS-WINDOWS
               '("Consolas" "Microsoft Yahei"))))
  
  (let* ((en-font      (or dotemacs-font (car fonts)))
         (cn-font      (or dotemacs-cn-font (car (cdr fonts))))
         (en-font-size (or dotemacs-font-size 11))
         (cn-font-size (or dotemacs-cn-font-size 16)))
    (set-face-attribute 'default nil :font
                        (format "%s %d" en-font en-font-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family cn-font :size cn-font-size)))
    ;; Fix chinese font width and rescale
    (setq face-font-rescale-alist '((cn-font . 1.2)))))

(defun dotemacs|init-fonts ()
  "Set the font."
  (add-to-list 'after-make-frame-functions
               (lambda (new-frame)
                 (select-frame new-frame)
                 (if (display-graphic-p)
                   (dotemacs|set-font))))
  
  (if (display-graphic-p)
    (dotemacs|set-font)))

(defun dotemacs*load-theme-hooks (theme &rest _)
  "Set up `dotemacs-load-theme-hook' to run after `load-theme' is called."
  (setq dotemacs-theme theme)
  (run-hooks 'dotemacs-load-theme-hook))
(advice-add #'load-theme :after #'dotemacs*load-theme-hooks)

(defun dotemacs|init-theme ()
  "Set the theme."
  (when (and dotemacs-theme (not (memq dotemacs-theme custom-enabled-themes)))
    (load-theme dotemacs-theme t)))

;; fonts
(add-hook 'dotemacs-init-ui-hook #'dotemacs|init-fonts)
;; themes
(add-hook 'dotemacs-init-ui-hook #'dotemacs|init-theme)

;;
;; Bootstrap

(defun dotemacs|init-ui ()
  "Initialize ui."
  (run-hook-wrapped 'dotemacs-init-ui-hook #'dotemacs-try-run-hook))

(add-hook 'emacs-startup-hook #'dotemacs|init-ui)

(provide 'core-ui)
;;; core-ui.el ends here
