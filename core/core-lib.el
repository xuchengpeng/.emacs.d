;;; core-lib.el --- Initialize core lib configurations. -*- lexical-binding: t; -*-
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
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let))))

;; alias with-eval-after-load
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load! 'with-eval-after-load)
  (defmacro after-load! (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;
;; Helpers
;;

(defun dotemacs-unquote (exp)
  "Return EXP unquoted."
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun dotemacs-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (if (listp exp) exp (list exp)))

(defun dotemacs-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun dotemacs-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type :test keyword)
  (substring (symbol-name keyword) 1))

(defun dotemacs--resolve-hook-forms (hooks)
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (dotemacs-enlist (dotemacs-unquote hooks))
           if (eq (car-safe hook) 'quote)
            collect (cadr hook)
           else if quoted-p
            collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

;;
;; Library
;;

(defun FILE! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        (buffer-file-name)
        ((stringp (car-safe current-load-list)) (car current-load-list))))

(defun DIR! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (let ((file (FILE!)))
    (and file (file-name-directory file))))

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defalias 'lambda! 'λ!)

(defmacro after! (targets &rest body)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation. This will no-op on features that have been disabled by the user."
  (declare (indent defun) (debug t))
  (unless (symbolp targets)
    (list (if (or (not (bound-and-true-p byte-compile-current-file))
                  (dolist (next (dotemacs-enlist targets))
                    (unless (keywordp next)
                      (if (symbolp next)
                          (require next nil :no-error)
                        (load next :no-message :no-error)))))
              #'progn
            #'with-no-warnings)
          (if (symbolp targets)
              `(with-eval-after-load ',targets ,@body)
            (pcase (car-safe targets)
              ((or :or :any)
               (macroexp-progn
                (cl-loop for next in (cdr targets)
                         collect `(after! ,next ,@body))))
              ((or :and :all)
               (dolist (next (cdr targets))
                 (setq body `((after! ,next ,@body))))
               (car body))
              (_ `(after! (:and ,@targets) ,@body)))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if dotemacs-debug-mode
       (progn ,@forms)
     (let ((old-fn (symbol-function 'write-region)))
       (cl-letf* ((standard-output (lambda (&rest _)))
                  ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                  ((symbol-function 'message) (lambda (&rest _)))
                  ((symbol-function 'write-region)
                   (lambda (start end filename &optional append visit lockname mustbenew)
                     (unless visit (setq visit 'no-message))
                     (funcall old-fn start end filename append visit lockname mustbenew)))
                  (inhibit-message t)
                  (save-silently t))
         ,@forms))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (dotemacs--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(if append-p (nreverse forms) forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as
`add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro setq-hook! (hooks &rest rest)
  "Convenience macro for setting buffer-local variables in a hook.

  (setq-hook! 'markdown-mode-hook
    line-spacing 2
    fill-column 80)"
  (declare (indent 1))
  (unless (= 0 (% (length rest) 2))
    (signal 'wrong-number-of-arguments (length rest)))
  `(add-hook! ,hooks
     ,@(let (forms)
         (while rest
           (let ((var (pop rest))
                 (val (pop rest)))
             (push `(setq-local ,var ,val) forms)))
         (nreverse forms))))

(provide 'core-lib)

;;; core-lib.el ends here
