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

(setq package-user-dir (expand-file-name (format "elpa-%s" emacs-version)
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
                             '(melpa emacs-china tuna)))))
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
     ((eq archives 'tuna)
      (setq package-archives `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
                               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
                               ,(cons "org"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))))
     (t
      (error "Unknown archives: '%s'" archives))))

  (message "Set package archives to '%s'." archives))

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

(defun dotemacs-get-packages (&optional installed-only-p)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend.

Warning: this function is expensive; it re-evaluates all of dotemacs's config files.
Be careful not to use it in a loop.

If INSTALLED-ONLY-P, only return packages that are installed."
  (cl-loop with packages = (append dotemacs-core-packages (mapcar #'car dotemacs-packages))
           for sym in (cl-delete-duplicates packages)
           if (and (or (not installed-only-p)
                       (package-installed-p sym))
                   (or (assq sym dotemacs-packages)
                       (and (assq sym package-alist)
                            (list sym))))
           collect it))

(defun dotemacs-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (let* ((inhibit-message (not dotemacs-debug-mode))
         (plist (or plist (cdr (assq name dotemacs-packages))))
         (recipe (plist-get plist :recipe))
         quelpa-upgrade-p)
    (if recipe
        (quelpa recipe)
      (package-install name))
    (when (package-installed-p name)
      (cl-pushnew (cons name plist) dotemacs-packages :test #'eq :key #'car)
      t)))

(defun dotemacs-install-packages ()
  "Install packages."
  (dolist (pkg (dotemacs-get-packages))
    (let* ((pkg-name (car pkg))
           (pkg-plist (cdr pkg)))
      (unless (package-installed-p pkg-name)
        (if (dotemacs-install-package pkg-name pkg-plist)
            (message "Emacs installed %s" pkg-name)
          (error "Emacs couldn't install %s" pkg-name))))))

(provide 'core-packages)
;;; core-packages.el ends here
