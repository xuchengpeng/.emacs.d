;;; core-ui.el --- Initialize core ui configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/.emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  "The default font to use.

Expects either a `font-spec', or a font string.

Examples:
  (setq dotemacs-font (font-spec :family \"Fira Mono\" :size 12))
  (setq dotemacs-font \"Terminus (TTF):pixelsize=12:antialias=off\")")

(defvar dotemacs-cn-font nil
  "The chinese font to use.")

;;
;;; General UX

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;;
;;; Cursor

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize emacs in steps, it looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(unless EMACS27+  ; We already do this in early-init.el
  ;; Disable tool and scrollbars; dotemacs encourages keyboard-centric workflows, so
  ;; these are just clutter (the scrollbar also impacts Emacs' performance).
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; _while_ we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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
  (when dotemacs-font
    (set-face-attribute 'default nil :font dotemacs-font))
  (when dotemacs-cn-font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset dotemacs-cn-font))))

(defun dotemacs|init-fonts ()
  "Set the font."
  (add-to-list 'after-make-frame-functions
               (lambda (new-frame)
                 (select-frame new-frame)
                 (when (display-graphic-p)
                   (dotemacs|set-font))))
  (when (display-graphic-p)
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

(add-hook 'window-setup-hook #'dotemacs|init-ui)

;;
;;; Fixes/hacks

;; Don't allow cursor to enter the prompt
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(provide 'core-ui)
;;; core-ui.el ends here
