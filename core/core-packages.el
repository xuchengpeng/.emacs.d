;; core-packages.el --- Initialize core packages configurations.
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
;; Core packages configurations.
;;

;;; Code:

(defvar dotemacs-core-packages '(use-package diminish bind-key)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar dotemacs-packages ()
  "A list of packages to install. Packages are represented by symbols.")

(defvar doteamcs-modules-path-list ()
  "The path to each modules.")

(setq package-user-dir (concat dotemacs-packages-dir "elpa/")
      load-prefer-newer t
      package-enable-at-startup nil)

(defvar-local package-archives-list '(melpa emacs-china tuna custom))
(defun dotemacs-set-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (interactive
   (list
    (intern (completing-read "Switch to archives: "
                             package-archives-list))))
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
     ((eq archives 'custom)
      (setq package-archives dotemacs-custom-package-archives))
     (t
      (error "Unknown archives: '%s'" archives))))

  (message "Set package archives to '%s'." archives))

(dotemacs-set-package-archives dotemacs-package-archives)

(defun dotemacs-install-packages (packages-list)
  (when-let* ((core-packages (cl-remove-if #'package-installed-p packages-list)))
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (package core-packages)
      (package-install package)
      (if (package-installed-p package)
          (message "dotemacs installed %s" package)
        (error "dotemacs couldn't install %s" package)))))

(defun dotemacs-ensure-packages-initialized ()
  "Make sure package.el is initialized."
    (require 'package)
    (package-initialize))

(defun dotemacs-ensure-core-packages ()
  "Make sure `dotemacs-core-packages' are installed."
  (dotemacs-install-packages dotemacs-core-packages)
  (setq use-package-verbose t))

(defun dotemacs-load-core-autoload ()
  "Load core autoload file."
  (let ((autoload-path (concat dotemacs-core-dir "autoload/")))
    (when (file-directory-p autoload-path)
      (let ((file-list (directory-files autoload-path t "^[^#\.].*el$")))
        (dolist (file file-list)
  	      (load file t t t))))))

(defun dotemacs-modules-load-autoload ()
  "Load autoload.el and autoload/*.el in each module."
  (dolist (m-path doteamcs-modules-path-list)
    (let ((path (concat m-path "autoload.el"))
          (autoload-path (concat m-path "autoload/")))
      (when (file-exists-p path)
        (load path t t t))
      (when (file-directory-p autoload-path)
        (let ((file-list (directory-files autoload-path t "^[^#\.].*el$")))
          (dolist (file file-list)
  	        (load file t t t)))))))

(defun dotemacs-modules-load-package ()
  "Load packages.el in each module."
  (dolist (m-path doteamcs-modules-path-list)
    (let ((path (concat m-path "packages.el")))
      (when (file-exists-p path)
        (load path t t t))))
  (dotemacs-install-packages dotemacs-packages))

(defun dotemacs-modules-load-config ()
  "Load config.el in each module."
  (dolist (m-path doteamcs-modules-path-list)
    (let ((path (concat m-path "config.el")))
      (if (file-exists-p path)
          (load path t t t)
        (message (format "%s does not exist!" path))))))

(defun dotemacs-initialize-modules ()
  (dotemacs-modules-load-autoload)
  (dotemacs-modules-load-package)
  (dotemacs-modules-load-config))

(defun dotemacs-keyword-to-name-str (keyword)
  "Remove the colon in KEYWORD symbol and turn it into string.

i.e. :keyword to \"keyword\"."
  (replace-regexp-in-string "^:" "" (symbol-name keyword)))

(defmacro dotemacs! (&rest modules-list)
  "Declare stars in MODULES-LIST.
Separate modules with sub-directories' name.
Basically adding modules path to `doteamcs-modules-path-list'.

Example: (dotemacs| :feature evil :ui custom) for modules/feature/evil
and modules/ui/custom."
  (dolist (module modules-list)
    (cond ((keywordp module) (setq mode module))
          ((not      mode) (error "No sub-folder specified in `dotemacs|' for %s" module))
          (t               (let ((module-path (format "%s%s/%s/" dotemacs-modules-dir (dotemacs-keyword-to-name-str mode) module)))
                             (add-to-list 'doteamcs-modules-path-list module-path t))))))

(defmacro package! (&rest packages-list)
  "Add packages in PACKAGE-LIST to ‘dotemacs-packages’.

Can take multiple packages.
e.g. (package! evil evil-surround)"
  `(dolist (package ',packages-list)
     (add-to-list 'dotemacs-packages package)))

(provide 'core-packages)

;;; core.el ends here
