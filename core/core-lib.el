;;; core-lib.el --- Initialize core lib configurations. -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;; Polyfills
(unless EMACS26+
  (with-no-warnings
    ;; `kill-current-buffer' was introduced in Emacs 26
    (defalias 'kill-current-buffer #'kill-this-buffer)
    ;; if-let and when-let were moved to (if|when)-let* in Emacs 26+ so we alias
    ;; them for 25 users.
    (defalias 'if-let* #'if-let)
    (defalias 'when-let* #'when-let)

    ;; `mapcan' was introduced in 26.1. `cl-mapcan' isn't a perfect replacement,
    ;; but it's close enough.
    (defalias 'mapcan #'cl-mapcan)

    (defun alist-get (key alist &optional default remove testfn)
      "Return the value associated with KEY in ALIST.
If KEY is not found in ALIST, return DEFAULT.
Use TESTFN to lookup in the alist if non-nil.  Otherwise, use `assq'.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
      (ignore remove) ;;Silence byte-compiler.
      (let ((x (if (not testfn)
                   (assq key alist)
                 ;; In Emacs<26, `assoc' has no testfn arg, so we have to
                 ;; implement it ourselves
                 (if testfn
                     (cl-loop for entry in alist
                              if (funcall testfn key entry)
                              return entry)
                   (assoc key alist)))))
        (if x (cdr x) default)))))

;; alias with-eval-after-load
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load! 'with-eval-after-load)
  (defmacro after-load! (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;
;; Public library

(defun dotemacs-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun dotemacs-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun dotemacs-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun dotemacs-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defmacro dotemacs-log (format-string &rest args)
  "Log to *Messages* if `dotemacs-debug-mode' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same arguments as `message'."
  `(when dotemacs-debug-mode
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "dotemacs " 'face 'font-lock-comment-face)
                 format-string)
        ,@args))))

(defalias 'dotemacs-partial #'apply-partially)

(defun dotemacs-rpartial (fn &rest args)
  "Return a function that is a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun dotemacs--resolve-path-forms (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (dotemacs--resolve-path-forms
    '(or A (and B C))
    \"~\")

Returns (approximately):

  '(let* ((_directory \"~\")
          (A (expand-file-name A _directory))
          (B (expand-file-name B _directory))
          (C (expand-file-name C _directory)))
     (or (and (file-exists-p A) A)
         (and (if (file-exists-p B) B)
              (if (file-exists-p C) C))))

This is used by `associate!', `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (cond ((stringp spec)
         `(let ((--file-- ,(if (file-name-absolute-p spec)
                             spec
                           `(expand-file-name ,spec ,directory))))
            (and (file-exists-p --file--)
                 --file--)))
        ((and (listp spec)
              (memq (car spec) '(or and)))
         `(,(car spec)
           ,@(cl-loop for i in (cdr spec)
                      collect (dotemacs--resolve-path-forms i directory))))
        ((or (symbolp spec)
             (listp spec))
         `(let ((--file-- ,(if (and directory
                                    (or (not (stringp directory))
                                        (file-name-absolute-p directory)))
                               `(expand-file-name ,spec ,directory)
                             spec)))
            (and (file-exists-p --file--)
                 --file--)))
        (spec)))

;;
;;; Helpers

(defun dotemacs--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (dotemacs-enlist (dotemacs-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun dotemacs--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (dotemacs--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "dotemacs--setq-%s-for-%s-h"
                                          var mode))))))

;;
;;; Sugars

(defmacro λ! (&rest body)
  "Expands to (lambda () (interactive) ,@body)."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))
(defalias 'lambda! 'λ!)

(defun λ!! (command &optional arg)
  "Expands to a command that interactively calls COMMAND with prefix ARG."
  (declare (doc-string 1))
  (lambda () (interactive)
     (let ((current-prefix-arg arg))
       (call-interactively command))))
(defalias 'lambda!! 'λ!!)

(defun file! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        ((stringp (car-safe current-load-list))
         (car current-load-list))
        (buffer-file-name)
        ((error "Cannot get this file-path"))))

(defun dir! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (when-let (path (file!))
    (directory-file-name (file-name-directory path))))

(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro nconcq! (sym &rest lists)
  "Append LISTS to SYM by altering them in place."
  `(setq ,sym (nconc ,sym ,@lists)))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is used."
  `(let ((default-directory ,(dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path))))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (intern (format "dotemacs--transient-%s-h" (sxhash hook-or-function)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hook(s) to be added to: either an unquoted mode, an unquoted list of
     modes, a quoted hook variable or a quoted list of hook variables. If
     unquoted, '-hook' will be appended to each symbol.
  3. The function(s) to be added: this can be one function, a list thereof, a
     list of `defun's, or body forms (implicitly wrapped in a closure).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (dotemacs--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'dotemacs-unquote rest)
                     (dotemacs-enlist (dotemacs-unquote (car rest))))))

            ((setq func-forms (list `(lambda () ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (dotemacs--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(remove-hook ',hook #',fn) ; ensure set order
            collect `(add-hook ',hook #',fn))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (dotemacs--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let* ((path (or path
                   (dir!)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                  `(expand-file-name ,filename ,path)
                filename)))
    `(condition-case-unless-debug e
         (let (file-name-handler-alist)
           (load ,file ,noerror 'nomessage))
       (dotemacs-error (signal (car e) (cdr e)))
       (error
        (let* ((source (file-name-sans-extension ,file))
               (err (cond ((not (featurep 'core))
                           (cons 'error (file-name-directory path)))
                          ((file-in-directory-p source dotemacs-core-dir)
                           (cons 'dotemacs-error dotemacs-core-dir))
                          ((file-in-directory-p source dotemacs-private-dir)
                           (cons 'dotemacs-private-error dotemacs-private-dir))
                          ((cons 'dotemacs-module-error dotemacs-dir)))))
          (signal (car err)
                  (list (file-relative-name
                         (concat source ".el")
                         (cdr err))
                        e)))))))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "dotemacs--delay-form-%s-h" (sxhash (cons condition body))))))
        `(progn
           (fset ',fn (lambda (&rest args)
                        (when ,(or condition t)
                          (remove-hook 'after-load-functions #',fn)
                          (unintern ',fn nil)
                          (ignore args)
                          ,@body)))
           (put ',fn 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fn)))))

(defmacro defer-feature! (feature &optional fn)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN runs.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if FN is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "dotemacs--defer-feature-%s-a" feature)))
        (fn (or fn feature)))
    `(progn
       (setq features (delq ',feature features))
       (advice-add #',fn :before #',advice-fn)
       (defun ,advice-fn (&rest _)
         ;; Some plugins (like yasnippet) will invoke a fn early to parse
         ;; code, which would prematurely trigger this. In those cases, well
         ;; behaved plugins will use `delay-mode-hooks', which we can check for:
         (when (and ,(intern (format "%s-hook" fn))
                    (not delay-mode-hooks))
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (advice-remove #',fn #',advice-fn))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load-file', `write-region' and anything that
writes to `standard-output'."
  `(cond (dotemacs-debug-mode ,@forms)
         ((not dotemacs-interactive-mode)
          (let ((old-fn (symbol-function 'write-region)))
            (cl-letf ((standard-output (lambda (&rest _)))
                      ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                      ((symbol-function 'message) (lambda (&rest _)))
                      ((symbol-function 'write-region)
                       (lambda (start end filename &optional append visit lockname mustbenew)
                         (unless visit (setq visit 'no-message))
                         (funcall old-fn start end filename append visit lockname mustbenew))))
              ,@forms)))
         ((let ((inhibit-message t)
                (save-silently t))
            (prog1 ,@forms (message ""))))))


;;
;;; Definers

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (dotemacs-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       ,(when where-alist
          `(dolist (targets (list ,@(nreverse where-alist)))
             (dolist (target (cdr targets))
               (advice-add target (car targets) #',symbol)))))))

(provide 'core-lib)
;;; core-lib.el ends here
