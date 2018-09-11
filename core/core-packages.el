;;; core-packages.el --- Initialize core packages configurations. -*- lexical-binding: t; -*-
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
;; Core packages configurations.
;;

;;; Code:

(defvar dotemacs-core-packages '(use-package diminish bind-key quelpa)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar dotemacs-packages ()
  "A list of enabled packages.")

(setq package-user-dir (expand-file-name (format "elpa-%s.%s"
                                                 emacs-major-version
                                                 emacs-minor-version)
                                         dotemacs-packages-dir)
      package-gnupghome-dir (expand-file-name "gnupg" dotemacs-packages-dir)
      package-enable-at-startup nil
      load-prefer-newer t
      use-package-verbose dotemacs-debug-mode
      use-package-compute-statistics dotemacs-debug-mode
      use-package-minimum-reported-time (if dotemacs-debug-mode 0 0.1)
      use-package-expand-minimally (not dotemacs-debug-mode)
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose dotemacs-debug-mode
      quelpa-dir (expand-file-name "quelpa" dotemacs-packages-dir))

(defun dotemacs/set-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (interactive
   (list
    (intern (completing-read "Switch to archives: "
                             '(melpa emacs-china netease tuna)))))
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (cond
     ((eq archives 'melpa)
      (setq package-archives `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
                               ,(cons "melpa" (concat proto "://melpa.org/packages/"))
                               ,(cons "org"   (concat proto "://orgmode.org/elpa/")))))
     ((eq archives 'emacs-china)
      (setq package-archives `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
                               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))
                               ,(cons "org"   (concat proto "://elpa.emacs-china.org/org/")))))
     ((eq archives 'netease)
      (setq package-archives `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
                               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))
                               ,(cons "org"   (concat proto "://mirrors.163.com/elpa/org/")))))
     ((eq archives 'tuna)
      (setq package-archives `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
                               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
                               ,(cons "org"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))))
     (t
      (error "Unknown archives: '%s'" archives)))))

(unless (eq dotemacs-package-archives 'custom)
  (dotemacs/set-package-archives dotemacs-package-archives))

;;
;; Functions
;;

(defun dotemacs-initialize-core ()
  "Initial core packages."
  (require 'package)
  (package-initialize)
  
  (unless package-archive-contents
    (package-refresh-contents))
  
  (when-let* ((core-packages (cl-remove-if #'package-installed-p dotemacs-core-packages)))
    (dolist (package core-packages)
      (let ((inhibit-message t))
        (package-install package))
      (if (package-installed-p package)
          (message "Emacs installed %s" package)
        (error "Emacs couldn't install %s" package))))
  
  (require 'use-package)
  (require 'quelpa))

(defun dotemacs-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (cl-check-type name symbol)
  (let* ((inhibit-message (not dotemacs-debug-mode))
         (plist (or plist (cdr (assq name dotemacs-packages)))))
    (if-let* ((recipe (plist-get plist :recipe)))
        (condition-case e
            (let (quelpa-upgrade-p)
              (quelpa recipe))
          ((debug error)
           (signal (car e) (cdr e))))
      (package-install name))
    (when (package-installed-p name)
      (setf (alist-get name dotemacs-packages) plist)
      name)))

(defun dotemacs-install-packages ()
  "Install packages defined by dotemacs-packages."
  (dolist (pkg (reverse dotemacs-packages))
    (let* ((pkg-name (car pkg))
           (pkg-plist (cdr pkg)))
      (unless (package-installed-p pkg-name)
        (if (dotemacs-install-package pkg-name pkg-plist)
            (message "Emacs installed %s" pkg-name)
          (error "Emacs couldn't install %s" pkg-name))))))

(provide 'core-packages)
;;; core-packages.el ends here
