;;; core-packages.el --- Initialize core packages configurations. -*- lexical-binding: t; -*-
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

(defvar dotemacs-modules ()
  "A hash table of enabled modules. Set by `dotemacs-initialize-modules'.")

(defvar dotemacs-core-packages '(use-package diminish bind-key)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar dotemacs-packages ()
  "A list of modules packages.")

(setq package-user-dir (concat dotemacs-packages-dir "elpa/")
      load-prefer-newer t
      package-enable-at-startup nil
      use-package-verbose dotemacs-debug-mode
      use-package-minimum-reported-time (if dotemacs-debug-mode 0 0.1))

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

;;
;; Functions
;;

(defun dotemacs-initialize-core ()
  "Make sure package.el is initialized."
  (require 'package)
  (package-initialize)
  
  (dotemacs-install-core-packages))

(defun dotemacs-install-core-packages ()
  "Make sure `dotemacs-core-packages' are installed."
  (dotemacs-install-packages dotemacs-core-packages))

(defun dotemacs-install-modules-packages ()
  "Make sure `dotemacs-packages' are installed."
  (dotemacs-install-packages dotemacs-packages))

(defun dotemacs-install-packages (packages-list)
  "Install packages defined by PACKAGES-LIST."
  (when-let* ((core-packages (cl-remove-if #'package-installed-p packages-list)))
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (package core-packages)
      (let ((inhibit-message t))
        (package-install package))
      (if (package-installed-p package)
          (message "dotemacs installed %s" package)
        (error "dotemacs couldn't install %s" package)))))

(defun dotemacs-initialize-autoload ()
  (if (file-exists-p dotemacs-autoload-file)
      (load dotemacs-autoload-file t (not dotemacs-debug-mode))
    (let ((targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" dotemacs-core-dir))))
      (dolist (path (dotemacs-module-paths))
        (let ((auto-dir  (expand-file-name "autoload" path))
              (auto-file (expand-file-name "autoload.el" path)))
          (when (file-exists-p auto-file)
            (push auto-file targets))
          (when (file-directory-p auto-dir)
            (dolist (file (directory-files-recursively auto-dir "\\.el$"))
              (push file targets)))))
      (dolist (file (reverse targets))
        (load file t (not dotemacs-debug-mode))))))

(defun dotemacs-generate-autoload-file ()
  "Generate the autoloads.el file, specified by `dotemacs-autoload-file'.

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates an autoloads file at the path specified
by `dotemacs-autoload-file'. This file tells Emacs where to find lazy-loaded
functions.

This should be run whenever init.el or an autoload file is modified."
  (interactive)
  
  (let ((targets
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" dotemacs-core-dir))))
      (dolist (path (dotemacs-module-paths))
        (let ((auto-dir  (expand-file-name "autoload" path))
              (auto-file (expand-file-name "autoload.el" path)))
          (when (file-exists-p auto-file)
            (push auto-file targets))
          (when (file-directory-p auto-dir)
            (dolist (file (directory-files-recursively auto-dir "\\.el$"))
              (push file targets)))))
      (when (file-exists-p dotemacs-autoload-file)
        (delete-file dotemacs-autoload-file)
        (message "Deleted old autoloads.el"))
      (dolist (file (reverse targets))
        (message
         (cond ((update-file-autoloads file nil dotemacs-autoload-file)
                "Nothing in %s")
               (t
                "Scanned %s"))
         (file-relative-name file dotemacs-dir)))
      (make-directory (file-name-directory dotemacs-autoload-file) t)
      (let ((buf (get-file-buffer dotemacs-autoload-file))
            current-sexp)
        (unwind-protect
            (condition-case-unless-debug ex
                (with-current-buffer buf
                  (save-buffer)
                  (goto-char (point-min))
                  (while (re-search-forward "^(" nil t)
                    (save-excursion
                      (backward-char)
                      (setq current-sexp (read (thing-at-point 'sexp t)))
                      (eval current-sexp t))
                    (forward-char))
                  (message "Finished generating autoloads.el!"))
              ('error
               (delete-file dotemacs-autoload-file)
               (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                      (nth 0 current-sexp)
                      (nth 1 current-sexp)
                      (car ex) (error-message-string ex))))
          (kill-buffer buf)))))

(defun dotemacs-load-modules-file (file)
  "Load packages.el in each module."
  (dolist (path (dotemacs-module-paths file))
    (load path t (not dotemacs-debug-mode))))

(defun dotemacs-initialize-modules ()
  "Initialize modules."
  (dotemacs-initialize-autoload)
  (dotemacs-load-modules-file "packages.el")
  (dotemacs-install-modules-packages)
  (dotemacs-load-modules-file "config.el")
  (message "dotemacs modules initialized"))

(defun dotemacs-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (expand-file-name (concat module "/" submodule "/" file)
                    dotemacs-modules-dir))

(defun dotemacs-module-from-path (path)
  "Get module cons cell (MODULE . SUBMODULE) for PATH, if possible."
  (when-let* ((path (file-relative-name (file-truename path) (file-truename dotemacs-modules-dir))))
    (let ((segments (split-string path "/")))
      (cons (intern (concat ":" (car segments)))
            (intern (cadr segments))))))

(defun dotemacs-module-paths (&optional append-file)
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (cl-loop for (module . submodule) in (dotemacs-module-pairs)
           for path = (dotemacs-module-path module submodule append-file)
           if (file-exists-p path)
           collect path))

(defun dotemacs-module-get (module submodule)
  "Returns a list of flags provided for MODULE SUBMODULE."
  (gethash (cons module submodule) dotemacs-modules))

(defun dotemacs-module-enabled-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `dotemacs-modules'."
  (and (dotemacs-module-get module submodule) t))

(defun dotemacs-module-enable (module submodule &optional flags)
  "Adds MODULE and SUBMODULE to `dotemacs-modules', overwriting it if it exists.

MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang 'emacs-lisp."
  (let ((key (cons module submodule)))
    (puthash key
             (or (dotemacs-enlist flags)
                 (gethash key dotemacs-modules)
                 '(t))
             dotemacs-modules)))

(defun dotemacs-module-pairs ()
  "Returns `dotemacs-modules' as a list of (MODULE . SUBMODULE) cons cells. The list
is sorted by order of insertion unless ALL-P is non-nil. If ALL-P is non-nil,
include all modules, enabled or otherwise."
  (unless (hash-table-p dotemacs-modules)
    (error "dotemacs-modules is uninitialized"))
  (cl-loop for key being the hash-keys of dotemacs-modules
           collect key))

;;
;; Macros
;;

(defmacro dotemacs! (&rest modules)
  "Adds MODULES to `dotemacs-modules'.

MODULES must be in mplist format.
e.g (dotemacs! :feature evil :lang emacs-lisp javascript java)"
  (unless dotemacs-modules
    (setq dotemacs-modules (make-hash-table :test #'equal
                                            :size (+ 5 (length modules))
                                            :rehash-threshold 1.0)))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m) (setq mode m))
            ((not mode)   (error "No namespace specified on `dotemacs!' for %s" m))
            ((listp m)    (dotemacs-module-enable mode (car m) (cdr m)))
            (t            (dotemacs-module-enable mode m)))))
  (dotemacs-initialize-modules))

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file (a string representing a directory path). If omitted,
the lookup is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (cl-assert (symbolp filesym) t)
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror
                 ,(not dotemacs-debug-mode))
        (unless noerror
          (error "Could not load file '%s' from '%s'" file path))))))

(defmacro require! (module submodule &optional flags reload-p)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (when (or reload-p (not (dotemacs-module-enabled-p module submodule)))
    (let ((module-path (dotemacs-module-path module submodule)))
      (if (not (file-directory-p module-path))
          (lwarn 'dotemacs-modules :warning "Couldn't find module '%s %s'"
                 module submodule)
        (dotemacs-module-enable module submodule flags)
        `(condition-case-unless-debug ex
             (load! config ,module-path t)
           ('error
            (lwarn 'dotemacs-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,module ',submodule
                   (error-message-string ex))))))))

(defmacro featurep! (module &optional submodule flag)
  "A convenience macro wrapper for `dotemacs-module-enabled-p'. It is evaluated at
compile-time/macro-expansion time."
  (unless submodule
    (let* ((path (or load-file-name byte-compile-current-file))
           (module-pair (dotemacs-module-from-path path)))
      (unless module-pair
        (error "featurep! couldn't detect what module I'm in! (in %s)" path))
      (setq flag module
            module (car module-pair)
            submodule (cdr module-pair))))
  (if flag
      (and (memq flag (dotemacs-module-get module submodule)) t)
    (dotemacs-module-enabled-p module submodule)))

(defmacro package! (&rest packages-list)
  "Add packages in PACKAGES-LIST to ‘dotemacs-packages’.

Can take multiple packages.
e.g. (package! evil evil-surround)"
  `(dolist (package ',packages-list)
     (add-to-list 'dotemacs-packages package t)))

;;
;; benchmark
;;

(defun dotemacs|display-benchmark (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "dotemacs loaded %s packages across %d modules in %.03fs"
           (length package-activated-list)
           (hash-table-size dotemacs-modules)
           (float-time (time-subtract (current-time) before-init-time))))
(add-hook 'dotemacs-init-hook #'dotemacs|display-benchmark)

(provide 'core-packages)

;;; core-packages.el ends here
