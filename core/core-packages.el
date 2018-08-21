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

(defvar dotemacs-modules ()
  "A hash table of enabled modules. Set by marco `dotemacs!'.")

(defvar dotemacs-modules-dirs
  (list (if dotemacs-private-dir (expand-file-name "modules/" dotemacs-private-dir))
        dotemacs-modules-dir)
  "A list of module root directories. Order determines priority.")

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
(defun dotemacs/set-package-archives (archives)
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

(dotemacs/set-package-archives dotemacs-package-archives)

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
          (message "Emacs installed %s" package)
        (error "Emacs couldn't install %s" package)))))

(defun dotemacs-initialize-autoload ()
  (if (file-exists-p dotemacs-autoload-file)
      (load dotemacs-autoload-file t (not dotemacs-debug-mode))
    (let ((targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" dotemacs-core-dir))))
      (dolist (path (dotemacs-module-load-path))
        (let ((auto-dir  (expand-file-name "autoload" path))
              (auto-file (expand-file-name "autoload.el" path)))
          (when (file-exists-p auto-file)
            (push auto-file targets))
          (when (file-directory-p auto-dir)
            (dolist (file (directory-files-recursively auto-dir "\\.el$"))
              (push file targets)))))
      (dolist (file (reverse targets))
        (load file t (not dotemacs-debug-mode))))))

(defun dotemacs/generate-autoload-file ()
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
      (dolist (path (dotemacs-module-load-path))
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

(defun dotemacs-initialize-modules ()
  "Initialize modules."
  (dotemacs-initialize-autoload)
  
  (maphash (lambda (key plist)
             (let ((path (plist-get plist :path)))
               (load (expand-file-name "packages.el" path) t (not dotemacs-debug-mode))))
           dotemacs-modules)
  (dotemacs-install-modules-packages)
  
  (maphash (lambda (key plist)
             (let ((path (plist-get plist :path)))
               (load (expand-file-name "init.el" path) t (not dotemacs-debug-mode))))
           dotemacs-modules)
  (run-hook-wrapped 'dotemacs-init-hook #'dotemacs-try-run-hook)
  
  (maphash (lambda (key plist)
             (let ((path (plist-get plist :path)))
               (load (expand-file-name "config.el" path) t (not dotemacs-debug-mode))))
           dotemacs-modules)
  (run-hook-wrapped 'dotemacs-post-init-hook #'dotemacs-try-run-hook)

  (message "Emacs modules initialized"))

;;
;; Module API
;;

(defun dotemacs-module-p (category module)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `dotemacs-modules')."
  (declare (pure t) (side-effect-free t))
  (and (hash-table-p dotemacs-modules)
       (gethash (cons category module) dotemacs-modules)
       t))

(defun dotemacs-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) dotemacs-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun dotemacs-module-put (category module property value &rest rest)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs."
  (when-let* ((plist (dotemacs-module-get category module)))
    (plist-put plist property value)
    (when rest
      (when (cl-oddp (length rest))
        (signal 'wrong-number-of-arguments (list (length rest))))
      (while rest
        (plist-put rest (pop rest) (pop rest))))
    (puthash (cons category module) plist dotemacs-modules)))

(defun dotemacs-module-set (category module &rest plist)
  "Enables a module by adding it to `dotemacs-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

Example:
  (dotemacs-module-set :lang 'haskell :flags '(+intero))"
  (when plist
    (let ((old-plist (dotemacs-module-get category module)))
      (unless (plist-member plist :flags)
        (plist-put plist :flags (plist-get old-plist :flags)))
      (unless (plist-member plist :path)
        (plist-put plist :path (or (plist-get old-plist :path)
                                   (dotemacs-module-locate-path category module))))))
  (puthash (cons category module)
           plist
           dotemacs-modules))

(defun dotemacs-module-path (category module &optional file)
  "Like `expand-file-name', but expands FILE relative to CATEGORY (keywordp) and
MODULE (symbol).

If the category isn't enabled this will always return nil. For finding disabled
modules use `dotemacs-module-locate-path'."
  (let ((path (dotemacs-module-get category module :path))
        file-name-handler-alist)
    (if file (expand-file-name file path)
      path)))

(defun dotemacs-module-locate-path (category &optional module file)
  "Searches `dotemacs-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path.

This doesn't require modules to be enabled. For enabled modules us
`dotemacs-module-path'."
  (when (keywordp category)
    (setq category (substring (symbol-name category) 1)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop with file-name-handler-alist = nil
           for default-directory in dotemacs-modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun dotemacs-module-from-path (&optional path)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path)."
  (let ((path (or path (FILE!))))
    (save-match-data
      (setq path (file-truename path))
      (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
        (when-let* ((category (match-string 1 path))
                    (module   (match-string 2 path)))
          (cons (dotemacs-keyword-intern category)
                (intern module)))))))

(defun dotemacs-module-load-path ()
  "Return a list of absolute file paths to activated modules."
  (declare (pure t) (side-effect-free t))
  (append (cl-loop for plist being the hash-values of (dotemacs-modules)
                   collect (plist-get plist :path))
          (if dotemacs-private-dir (list dotemacs-private-dir))))

(defun dotemacs-modules (&optional refresh-p)
  "Minimally initialize `dotemacs-modules' (a hash table) and return it."
  (or (unless refresh-p dotemacs-modules)
      (let ((noninteractive t)
            (dotemacs-modules
             (make-hash-table :test #'equal
                              :size 20
                              :rehash-threshold 1.0)))
        dotemacs-modules)))

;;
;; Macros
;;

(defmacro dotemacs! (&rest modules)
  "Adds MODULES to `dotemacs-modules'.

MODULES must be in mplist format.
e.g (dotemacs! :feature evil :lang emacs-lisp javascript java)"
  (unless dotemacs-modules
    (setq dotemacs-modules
          (make-hash-table :test #'equal
                           :size (if modules (length modules) 100)
                           :rehash-threshold 1.0)))
  (let (category m)
    (while modules
      (setq m (pop modules))
      (cond ((keywordp m) (setq category m))
            ((not category) (error "No module category specified for %s" m))
            ((catch 'dotemacs-modules
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (if-let* ((path (dotemacs-module-locate-path category module)))
                     (dotemacs-module-set category module :flags flags :path path)
                   (message "Warning: couldn't find the %s %s module" category module)))))))
    `(setq dotemacs-modules ',dotemacs-modules))
  (dotemacs-initialize-modules))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (unless path
    (setq path (or (DIR!)
                   (error "Could not detect path to look for '%s' in"
                          filename))))
  (let ((file (if path `(expand-file-name ,filename ,path) filename)))
    (if (file-exists-p file)
        `(load ,file ,noerror ,(not dotemacs-debug-mode))
      (unless noerror
        (error "Could not load file '%s' from '%s'" file path)))))

(defmacro require! (category module &rest plist)
  "Loads the module specified by CATEGORY (a keyword) and MODULE (a symbol)."
  `(let ((module-path (dotemacs-module-locate-path ,category ',module)))
     (dotemacs-module-set ,category ',module ,@plist)
     (if (directory-name-p module-path)
         (condition-case-unless-debug ex
             (load! "config" module-path :noerror)
           ('error
            (lwarn 'dotemacs-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,category ',module
                   (error-message-string ex))))
       (warn 'dotemacs-modules :warning "Couldn't find module '%s %s'"
             ,category ',module))))

(defmacro featurep! (category &optional module flag)
  "Returns t if CATEGORY MODULE is enabled. If FLAG is provided, returns t if
CATEGORY MODULE has FLAG enabled.

  (featurep! :config default)

Module FLAGs are set in your config's `dotemacs!' block, typically in
~/.emacs.d/init.el. Like so:

  :config (default +flag1 -flag2)

When this macro is used from inside a module, CATEGORY and MODULE can be
omitted. eg. (featurep! +flag1)"
  (and (cond (flag (memq flag (dotemacs-module-get category module :flags)))
             (module (dotemacs-module-p category module))
             ((let ((module-pair (dotemacs-module-from-path (FILE!))))
                (unless module-pair
                  (error "featurep! couldn't detect what module its in! (in %s)" (FILE!)))
                (memq category (dotemacs-module-get (car module-pair) (cdr module-pair) :flags)))))
       t))

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
           "Emacs loaded %s packages across %d modules in %.03fs"
           (length package-activated-list)
           (if dotemacs-modules (hash-table-count dotemacs-modules) 0)
           (float-time (time-subtract (current-time) before-init-time))))
(add-hook 'dotemacs-post-init-hook #'dotemacs|display-benchmark)

(provide 'core-packages)

;;; core-packages.el ends here
