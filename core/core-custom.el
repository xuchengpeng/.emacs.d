;;; core-custom.el --- Customizations. -*- lexical-binding: t; -*-
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
;; Customizations.
;;

;;; Code:

(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'convenience)

(defcustom dotemacs-full-name "Chuck"
  "Set user full name."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-mail-address "xucp@outlook.com"
  "Set user email address."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)
          (const :tag "Custom" custom))
  :group 'dotemacs)

(defcustom dotemacs-color-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          symbol)
  :group 'dotemacs)

(defcustom dotemacs-company-enable-yas nil
  "Enable/disable(t/nil) yasnippet for company backends."
  :type 'boolean
  :group 'dotemacs)

(provide 'core-custom)

;;; core-custom.el ends here
