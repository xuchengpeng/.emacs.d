;; init-ui.el --- Initialize UI configurations.
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/emacs.d

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
;; UI configurations.
;;

;;; Code:

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode)

;; (global-linum-mode t)
(if (fboundp 'display-line-numbers-mode)
    (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  (use-package nlinum
    :hook (after-init . global-nlinum-mode)
    ))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)

(defun dotemacs-set-custom-theme (theme)
  "Set dotemacs-themes"
  (require 'dotemacs-themes)
  
  ;; Global settings (defaults)
  (setq dotemacs-themes-enable-bold t    ; if nil, bold is universally disabled
        dotemacs-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Load the theme (dotemacs-one, dotemacs-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme theme t)
  
  ;; Enable flashing mode-line on errors
  (dotemacs-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (dotemacs-themes-org-config)
  
  (message "Set custom color theme '%s'." theme))

(defun dotemacs-set-theme (theme)
  "Set color theme."
  (cond
   ((eq theme 'default)
    (dotemacs-set-custom-theme 'dotemacs-one))
  
   ((eq theme 'dark)
    (load-theme 'sanityinc-tomorrow-night t))
  
   ((eq theme 'light)
    (load-theme 'sanityinc-tomorrow-day t))
   
   ((string-prefix-p "dotemacs" (symbol-name theme))
     (dotemacs-set-custom-theme theme))
   
   (t
    (error "Unknown color theme: '%s'" theme))))

;; Color theme
(dotemacs-set-theme dotemacs-color-theme)

;; modeline configurations
(require 'init-modeline)

;; (defun dotemacs-mode-line-fill (face reserve)
;;   "Return empty space using FACE and leaving RESERVE space on the right."
;;   (unless reserve
;;     (setq reserve 20))
;;   (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (propertize " "
;;               'display `((space :align-to
;;                                 (- (+ right right-fringe right-margin) ,reserve)))
;;               'face face))
;; 
;; (defun dotemacs-buffer-encoding-abbrev ()
;;   "The line ending convention used in the buffer."
;;   (let ((buf-coding (format "%s" buffer-file-coding-system)))
;;     (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
;;         (match-string 1 buf-coding)
;;       buf-coding)))
;; 
;; (setq-default mode-line-format
;;                 (list
;;                  "%e"
;;                  mode-line-front-space
;;                  ;; mode-line-mule-info
;;                  ;; mode-line-client
;;                  ;; mode-line-modified
;;                  ;; mode-line-remote
;;                  ;; mode-line-frame-identification
;;                  " "
;;                  ;; mode-line-buffer-identification
;;                  '(:eval (propertize "%b" 'face 'font-lock-keyword-face
;;                                      'help-echo (buffer-file-name)))
;;                  
;;                  " [" ;; insert vs overwrite mode, input-method in a tooltip
;;                  '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;                                      'face 'font-lock-preprocessor-face
;;                                      'help-echo (concat "Buffer is in "
;;                                                         (if overwrite-mode
;;                                                             "overwrite"
;;                                                           "insert") " mode")))
;; 
;;                  ;; was this buffer modified since the last save?
;;                  '(:eval (when (buffer-modified-p)
;;                            (concat "," (propertize "Mod"
;;                                                    'face 'font-lock-warning-face
;;                                                    'help-echo "Buffer has been modified"))))
;; 
;;                  ;; is this buffer read-only?
;;                  '(:eval (when buffer-read-only
;;                            (concat "," (propertize "RO"
;;                                                    'face 'font-lock-type-face
;;                                                    'help-echo "Buffer is read-only"))))
;;                  "] "
;;                  
;;                  "["
;;                  (propertize "%p" 'face 'font-lock-constant-face)
;;                  "/"
;;                  (propertize "%I" 'face 'font-lock-constant-face)
;;                  "] "
;;                  
;;                  mode-line-modes
;;                  
;;                  "   "
;;                  '(:eval `(vc-mode vc-mode))
;;                  "   "
;;                  
;;                  ;; (dotemacs-mode-line-fill 'mode-line 35)
;;                  
;;                  ;;mode-line-position
;;                  " ("
;;                  (propertize "%l" 'face 'font-lock-type-face)
;;                  ","
;;                  (propertize "%c" 'face 'font-lock-type-face)
;;                  ") "
;;                  
;;                  '(:eval (dotemacs-buffer-encoding-abbrev))
;;                  "  "
;;                  '(:eval mode-line-misc-info)
;;                  
;;                  mode-line-end-spaces
;;                  ))

;; (use-package powerline
;;   :config
;;   (setq powerline-default-separator 'arrow)
;;   (powerline-default-theme)
;;   )
;; 
;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme)
;;   )
;; 
;; (use-package smart-mode-line
;;   :config
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/theme 'respectful)
;;   (sml/setup)
;;   )

;; font configurations
;; Solution 1
(defun dotemacs-font-existsp (font)
  "Detect if a font exists"
  (if (null (x-list-fonts font))
      nil
    t))
;; or
;; (defun dotemacs-font-existsp (font)
;;   "Detect if a font exists"
;;   (if (find-font (font-spec :family font))
;;         t
;;       nil))

(defun dotemacs-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun dotemacs-set-english-chinese-font (english-fonts
                                          english-font-size
                                          chinese-fonts
                                          &optional chinese-font-size chinese-fonts-scale)
  (setq chinese-font-size (or chinese-font-size 16)
        chinese-fonts-scale (or chinese-fonts-scale 1.2))

  "english-font-size could be set to \":pixelsize=18\" or a integer.
   If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl) ; for find if
  (let ((en-font (dotemacs-make-font-string
                  (find-if #'dotemacs-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'dotemacs-font-existsp chinese-fonts)
                            :size chinese-font-size)))

    ;; Set English font
    ;; (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font)))
    
    ;; Fix chinese font width and rescale
    (setq face-font-rescale-alist '(("STHeiti" . ,chinese-fonts-scale)
                                    ("STFangsong" . ,chinese-fonts-scale)
                                    ("Microsoft Yahei" . ,chinese-fonts-scale))))

(defun dotemacs-set-font()
  (dotemacs-set-english-chinese-font
    '("DejaVu Sans Mono" "Source Code Pro" "Monaco" "Consolas") 11
    '("STXihei" "STHeiti" "STFangsong" "STZhongsong" "Microsoft Yahei" "����" "������" "����") 16))

;; Solution 2
;; (defun dotemacs-set-font()
;;   (setq fonts
;;         (cond ((eq system-type 'darwin)     '("Monaco"           "STHeiti"))
;;               ((eq system-type 'gnu/linux)  '("Menlo"            "WenQuanYi Zen Hei"))
;;               ((eq system-type 'windows-nt) '("DejaVu Sans Mono" "Microsoft Yahei"))))
;;   (set-face-attribute 'default nil :font
;;                       (format "%s:pixelsize=%d" (car fonts) 14))
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (font-spec :family (car (cdr fonts)) :size 16)))
;;   ;; Fix chinese font width and rescale
;;   (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2)))
;;   )

;; Solution 3
;; (defun dotemacs-set-font()
;;   
;;   ;; Setting English Font
;;   (when (member "DejaVu Sans Mono" (font-family-list))
;;     (set-face-attribute 'default nil :font
;;                         (format "%s:pixelsize=%d" "DejaVu Sans Mono" 14))
;;     )
;;   
;;   ;; Setting Chinese font
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset
;;                       (font-spec :family "Microsoft Yahei" :size 16))
;;     )
;;   
;;   ;; Fix chinese font width and rescale
;;   (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2)))
;;   )

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if (display-graphic-p)
                   (dotemacs-set-font))))

(if (display-graphic-p)
    (dotemacs-set-font))

(provide 'init-ui)

;;; init-ui.el ends here
