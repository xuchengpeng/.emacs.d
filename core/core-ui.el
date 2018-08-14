;;; core-ui.el --- Initialize core ui configurations. -*- lexical-binding: t; -*-
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
;; Core ui configurations.
;;

;;; Code:

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode)

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

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)

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
  "Make font string with FONT-NAME and FONT-SIZE."
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun dotemacs-set-english-chinese-font (english-fonts
                                          english-font-size
                                          chinese-fonts
                                          &optional chinese-font-size chinese-fonts-scale)
  "Set english(with ENGLISH-FONTS and ENGLISH-FONT-SIZE) and chinese(with CHINESE-FONTS,
CHINESE-FONT-SIZE and CHINESE-FONTS-SCALE) font."
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
  "Set fonts."
  (dotemacs-set-english-chinese-font
    '("DejaVu Sans Mono" "Source Code Pro" "Monaco" "Consolas") 11
    '("STXihei" "STHeiti" "STFangsong" "STZhongsong" "Microsoft Yahei" "黑体" "新宋体" "宋体") 16))

;; Solution 2
;;
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
;;
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


(provide 'core-ui)

;;; core-ui.el ends here
