;;; init.el --- Chuck's Emacs Configuration.
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
;; Chuck's Emacs Configuration.
;;

;;; Code:

(when (version< emacs-version "25.2")
  (error "Emacs version should be 25.2 or higher"))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq garbage-collection-messages t)
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(defvar dotemacs-dir (file-truename user-emacs-directory)
  "The path to this emacs.d directory.")
(defvar dotemacs-lisp-dir (expand-file-name "lisp" dotemacs-dir)
  "Where essential files are stored.")
(defvar dotemacs-themes-dir (expand-file-name "themes" dotemacs-dir)
  "Where themes files are stored.")
(defvar dotemacs-personal-dir (expand-file-name "personal" dotemacs-dir)
  "This directory is for your personal configuration.
Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar dotemacs-personal-preload-dir (expand-file-name "preload" dotemacs-personal-dir)
  "This directory is for your personal configuration, that you want loaded before dotemacs.")
(defvar dotemacs-cache-directory (expand-file-name ".cache/" dotemacs-dir))

(add-to-list 'load-path dotemacs-lisp-dir)
(add-to-list 'load-path dotemacs-themes-dir)

;; preload the personal settings from `dotemacs-personal-preload-dir'
(when (file-exists-p dotemacs-personal-preload-dir)
  (message "Loading personal configuration files in %s..." dotemacs-personal-preload-dir)
  (mapc 'load (directory-files dotemacs-personal-preload-dir 't "^[^#\.].*el$")))

(require 'init-custom)
(require 'init-packages)
(require 'init-core)
(require 'init-ui)
(require 'init-editor)

(require 'init-utils)
;; (require 'init-helm)
;; ivy & swiper & counsel
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-highlight)
(require 'init-projectile)

(require 'init-eshell)
(require 'init-flycheck)
(require 'init-magit)

(require 'init-org)
(require 'init-program)

(require 'server)
(unless (server-running-p)
  (server-start))

;; load the personal settings from `dotemacs-personal-dir`
(when (file-exists-p dotemacs-personal-dir)
  (message "Loading personal configuration files in %s..." dotemacs-personal-dir)
  (mapc 'load (directory-files dotemacs-personal-dir 't "^[^#\.].*el$")))

(provide 'init)

;;; init.el ends here
