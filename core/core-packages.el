;;; core-packages.el --- Initialize core packages configurations. -*- lexical-binding: t; -*-
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
;; Core packages configurations.
;;

;;; Code:

(defvar dotemacs-core-packages '(straight use-package async diminish bind-key general)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar dotemacs-core-package-sources
  '((org-elpa :local-repo nil)
    (melpa
     :type git :host github
     :repo "melpa/melpa"
     :no-build t)
    (gnu-elpa-mirror
     :type git :host github
     :repo "emacs-straight/gnu-elpa-mirror"
     :no-build t)
    (emacsmirror-mirror
     :type git :host github
     :repo "emacs-straight/emacsmirror-mirror"
     :no-build t))
  "A list of recipes for straight's recipe repos.")

(defvar dotemacs-packages ()
  "A list of enabled packages.")

(eval-and-compile
  (setq package-user-dir (expand-file-name "elpa" dotemacs-packages-dir)
        package-gnupghome-dir (expand-file-name "gnupg" dotemacs-packages-dir)
        package-enable-at-startup nil
        load-prefer-newer t
        use-package-verbose dotemacs-debug-mode
        use-package-compute-statistics dotemacs-debug-mode
        use-package-minimum-reported-time (if dotemacs-debug-mode 0 0.1)
        use-package-expand-minimally (not dotemacs-debug-mode)))

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

;;; straight
(setq straight-base-dir dotemacs-local-dir
      straight-repository-branch "develop"
      straight-vc-git-default-clone-depth 1
      straight-recipes-emacsmirror-use-mirror t)

(defun dotemacs--finalize-straight ()
  (mapc #'funcall (delq nil (mapcar #'cdr straight--transaction-alist)))
  (setq straight--transaction-alist nil))


(defun dotemacs-ensure-straight ()
  "Ensure `straight' is installed and was compiled with this version of Emacs."
  (defvar bootstrap-version)
  (let* ((user-emacs-directory straight-base-dir)
         (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
         (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;
;; Functions
;;

(defun dotemacs-initialize-packages (&optional force-p)
  "Initial core packages."
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (dotemacs-log "Initializing package.el")
    (require 'package)
    (package-initialize))
  
  (dotemacs-log "Initializing straight")
  (dotemacs-ensure-straight)
  (require 'straight)
  
  (straight--reset-caches)
  (mapc #'straight-use-recipes dotemacs-core-package-sources)
  (straight-register-package
   `(straight :type git :host github
              :repo ,(format "%s/straight.el" straight-repository-user)
              :files ("straight*.el")
              :branch ,straight-repository-branch
              :no-byte-compile t))
  (dotemacs-log "Initializing dotemacs-packages")
  (mapc #'straight-use-package dotemacs-core-packages)
  
  (unless dotemacs-interactive-mode
    (add-hook 'kill-emacs-hook #'dotemacs--finalize-straight)))

(defun dotemacs-install-packages (packages-list)
  "Install packages defined by PACKAGES-LIST."
  (mapc #'straight-use-package packages-list))

(provide 'core-packages)
;;; core-packages.el ends here
