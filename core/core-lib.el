;;; core-lib.el --- Initialize core lib configurations. -*- lexical-binding: t; -*-
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
;; Core lib configurations.
;;

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defconst EMACS26+
  (eval-when-compile (not (version< emacs-version "26"))))
(defconst EMACS27+
  (eval-when-compile (not (version< emacs-version "27"))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(eval-and-compile
  (unless EMACS26+
    (with-no-warnings
      ;; if-let and when-let are deprecated in Emacs 26+ in favor of their
      ;; if-let* variants, so we alias them for 25 users.
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let))))

;; alias with-eval-after-load
(if (fboundp 'with-eval-after-load)
    (defalias 'dotemacs-after-load 'with-eval-after-load)
  (defmacro dotemacs-after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun dotemacs-add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any output."
  `(let ((old-fn (symbol-function 'write-region)))
     (cl-letf* ((standard-output (lambda (&rest _)))
                ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                ((symbol-function 'message) (lambda (&rest _)))
                ((symbol-function 'write-region)
                 (lambda (start end filename &optional append visit lockname mustbenew)
                   (unless visit (setq visit 'no-message))
                   (funcall old-fn start end filename append visit lockname mustbenew)))
                (inhibit-message t)
                (save-silently t))
       ,@forms)))

(provide 'core-lib)

;;; core-lib.el ends here
