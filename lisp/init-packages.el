;; init-packages.el --- Initialize packages configurations.
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
;; Packages configurations.
;;

;;; Code:

(require 'cl)
(require 'package)

(when dotemacs-benchmark-enabled
  (require 'init-benchmark))

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

(setq package-user-dir (expand-file-name "elpa" dotemacs-dir))

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar dotemacs-packages
  '(ace-window
    aggressive-indent
    amx
    avy
    bind-key
    color-theme-sanityinc-tomorrow
    company
    counsel
    counsel-projectile
    ;; dashboard
    diff-hl
    diminish
    eshell
    exec-path-from-shell
    expand-region
    flycheck
    ;; helm
    ;; helm-projectile
    ;; helm-swoop
    ;; highlight-symbol
    hydra
    ivy
    ivy-hydra
    ;; ivy-prescient
    ivy-rich
    js2-mode
    json-mode
    magit
    magithub
    magit-todos
    markdown-mode
    multiple-cursors
    ;; neotree
    nlinum
    ;; org-plus-contrib
    ;; ox-hugo
    package-utils
    ;; powerline
    ;; prescient
    projectile
    ;; pt
    rainbow-delimiters
    rainbow-mode
    shell-pop
    ;; smart-mode-line
    smartparens
    ;; smex
    ;; spaceline
    swiper
    symbol-overlay
    toml-mode
    treemacs
    treemacs-projectile
    undo-tree
    use-package
    web-mode
    which-key
    ;; winum
    yaml-mode
    yasnippet)
  "A list of packages to ensure are installed at launch.")

;; update the package metadata is the local cache is missing
;; (unless package-archive-contents
;;   (package-refresh-contents))

(defun dotemacs-packages-installed-p ()
  "Check if all packages in `dotemacs-packages' are installed."
  (every #'package-installed-p dotemacs-packages))

(defun dotemacs-install-packages ()
  "Install all packages listed in `dotemacs-packages'."
  (unless (dotemacs-packages-installed-p)
    (message "%s" "dotemacs is now refreshing its package database...")
    (package-refresh-contents)
    (dolist (package dotemacs-packages)
      (unless (package-installed-p package)
        (package-install package)))))

;; run package installation
(dotemacs-install-packages)

(setq use-package-verbose t)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(use-package package-utils
  :commands (package-utils-list-upgrade package-utils-upgrade-all))

(provide 'init-packages)

;;; init-packages.el ends here
