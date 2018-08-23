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

(defvar dotemacs-load-theme-hook nil
  "Hook run when the theme is initialized.")

(defvar dotemacs-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(defun dotemacs|init-fonts ()
  "Set the font."
  (add-to-list 'after-make-frame-functions
               (lambda (new-frame)
                 (select-frame new-frame)
                 (if (display-graphic-p)
                   (dotemacs-set-font))))
  
  (if (display-graphic-p)
    (dotemacs-set-font)))

(use-package dotemacs-themes
  :load-path "themes"
  :defer t
  :init
  (unless dotemacs-theme
    (setq dotemacs-theme 'dotemacs-one))
  :config
  ;; Enable flashing mode-line on errors
  (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-org-config)
  
  ;; For treemacs users
  (when (fboundp 'treemacs)
    (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-treemacs-config))
  ;; or for neotree users
  (when (fboundp 'neotree)
    (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-neotree-config)))

(defun dotemacs*load-theme-hooks (theme &rest _)
  "Set up `dotemacs-load-theme-hook' to run after `load-theme' is called."
  (run-hooks 'dotemacs-load-theme-hook))
(advice-add #'load-theme :after #'dotemacs*load-theme-hooks)

(defun dotemacs|init-theme ()
  "Set the theme."
  (when dotemacs-theme
    (when (string-prefix-p "dotemacs" (symbol-name dotemacs-theme))
      (require 'dotemacs-themes))
    (load-theme dotemacs-theme t)))

;; fonts
(add-hook 'dotemacs-init-ui-hook #'dotemacs|init-fonts)
;; themes
(add-hook 'dotemacs-init-ui-hook #'dotemacs|init-theme)

(defun dotemacs|init-ui ()
  "Initialize ui."
  (run-hook-wrapped 'dotemacs-init-ui-hook #'dotemacs-try-run-hook))

(add-hook 'emacs-startup-hook #'dotemacs|init-ui)

(provide 'core-ui)
;;; core-ui.el ends here
