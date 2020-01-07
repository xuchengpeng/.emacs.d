;;; core-modules.el --- Core modules. -*- lexical-binding: t; -*-

(defvar dotemacs-modules ()
  "A hash table of enabled modules. Set by marco `dotemacs!'.")

(defvar dotemacs-modules-dirs
  (list dotemacs-private-dir
        dotemacs-modules-dir)
  "A list of module root directories. Order determines priority.")

(defvar dotemacs-init-time nil
  "The time it took, in seconds, for Emacs to initialize.")

;;
;;; Custom hooks

(defvar dotemacs-before-init-modules-hook nil
  "A list of hooks run before modules' config.el files are loaded, but after their init.el files are loaded.")

(defvar dotemacs-init-modules-hook nil
  "A list of hooks that run after modules' config.el files have loaded.")

(defvaralias 'dotemacs-after-init-modules-hook 'after-init-hook)

;;
;;; Functions

(defun dotemacs-initialize-modules ()
  "Initialize modules."
  (unless (hash-table-p dotemacs-modules)
      (setq dotemacs-modules (make-hash-table :test 'equal)))
  
  (dotemacs-initialize-autoload)
  
  (maphash (lambda (key plist)
             (load! "packages" (plist-get plist :path) t))
           dotemacs-modules)
  (when dotemacs-private-dir
    (load! "packages" dotemacs-private-dir t))
  (dotemacs-install-packages dotemacs-packages)
  
  (maphash (lambda (key plist)
             (load! "init" (plist-get plist :path) t))
           dotemacs-modules)
  (when dotemacs-private-dir
    (load! "init" dotemacs-private-dir t))
  (run-hook-wrapped 'dotemacs-before-init-modules-hook #'dotemacs-try-run-hook)
  
  (maphash (lambda (key plist)
             (load! "config" (plist-get plist :path) t))
           dotemacs-modules)
  (when dotemacs-private-dir
    (load! "config" dotemacs-private-dir t))
  (run-hook-wrapped 'dotemacs-init-modules-hook #'dotemacs-try-run-hook))

(defun dotemacs-initialize-autoload ()
  "Initialize autoloads."
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

;;
;;; Module API

(defun dotemacs-module-p (category module &optional flag)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `dotemacs-modules')."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) dotemacs-modules))
    (or (null flag)
        (and (memq flag (plist-get plist :flags))
             t))))

(defun dotemacs-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) dotemacs-modules))
    (if property
        (plist-get plist property)
      plist)))

(defun dotemacs-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (if-let ((old-plist (dotemacs-module-get category module)))
      (progn
        (when plist
          (when (cl-oddp (length plist))
            (signal 'wrong-number-of-arguments (list (length plist))))
          (while plist
            (plist-put old-plist (pop plist) (pop plist))))
        (puthash (cons category module) old-plist dotemacs-modules))
    (puthash (cons category module) plist dotemacs-modules)))

(defun dotemacs-module-set (category module &rest plist)
  "Enables a module by adding it to `dotemacs-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

Example:
  (dotemacs-module-set :lang 'haskell :flags '(+intero))"
  (puthash (cons category module)
           plist
           dotemacs-modules))

(defun dotemacs-module-path (category module &optional file)
  "Like `expand-file-name', but expands FILE relative to CATEGORY (keywordp) and
MODULE (symbol).

If the category isn't enabled this will always return nil. For finding disabled
modules use `dotemacs-module-locate-path'."
  (let ((path (dotemacs-module-get category module :path)))
    (if file
        (let (file-name-handler-alist)
          (expand-file-name file path))
      path)))

(defun dotemacs-module-locate-path (category &optional module file)
  "Searches `dotemacs-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path.

This doesn't require modules to be enabled. For enabled modules us
`dotemacs-module-path'."
  (when (keywordp category)
    (setq category (dotemacs-keyword-name category)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop with file-name-handler-alist = nil
           for default-directory in dotemacs-modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun dotemacs-module-from-path (&optional path enabled-only)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path).
If ENABLED-ONLY, return nil if the containing module isn't enabled."
  (if (null path)
      (ignore-errors
        (dotemacs-module-from-path (file!)))
    (let* ((file-name-handler-alist nil)
           (path (file-truename (or path (file!)))))
      (save-match-data
        (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
               (when-let* ((category (dotemacs-keyword-intern (match-string 1 path)))
                           (module   (intern (match-string 2 path))))
                 (and (or (null enabled-only)
                          (dotemacs-module-p category module))
                      (cons category module))))
              ((file-in-directory-p path dotemacs-core-dir)
               (cons :core (intern (file-name-base path))))
              ((file-in-directory-p path dotemacs-private-dir)
               (cons :private (intern (file-name-base path)))))))))

(defun dotemacs-module-load-path ()
  "Return a list of absolute file paths to activated modules."
  (declare (pure t) (side-effect-free t))
  (append (list dotemacs-private-dir)
          (cl-loop for plist being the hash-values of (dotemacs-modules)
                   collect (plist-get plist :path))
          nil))

(defun dotemacs-modules (&optional refresh-p)
  "Minimally initialize `dotemacs-modules' (a hash table) and return it.
This value is cached. If REFRESH-P, then don't use the cached value."
  (or (unless refresh-p dotemacs-modules)
      (let (dotemacs-interactive-mode
            dotemacs-modules)
        (load! "init" dotemacs-private-dir t)
        (or dotemacs-modules
            (make-hash-table :test 'equal
                             :size 20
                             :rehash-threshold 1.0)))))

;;
;;; Module config macros

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro dotemacs! (&rest modules)
  "Adds MODULES to `dotemacs-modules'.

MODULES must be in mplist format.
e.g (dotemacs! :feature evil :lang emacs-lisp javascript java)"
  `(let ((modules
          ,@(if (keywordp (car modules))
               (list (list 'quote modules))
             modules)))
    (unless dotemacs-modules
      (setq dotemacs-modules
            (make-hash-table :test 'equal
                             :size (if modules (length modules) 150)
                             :rehash-threshold 1.0)))
    (let (category m)
      (while modules
        (setq m (pop modules))
        (cond ((keywordp m)
               (setq category m))
              ((not category)
               (error "No module category specified for %s" m))
              ((and (listp m) (keywordp (car m)))
               (pcase (car m)
                 (:cond
                  (cl-loop for (cond . mods) in (cdr m)
                           if (eval cond t)
                           return (prependq! modules mods)))
                 (:if (if (eval (cadr m) t)
                          (push (caddr m) modules)
                        (prependq! modules (cdddr m))))
                 (fn (if (or (eval (cadr m) t)
                             (eq fn :unless))
                         (prependq! modules (cddr m))))))
              ((catch 'dotemacs-modules
                 (let* ((module (if (listp m) (car m) m))
                        (flags  (if (listp m) (cdr m))))
                   (if-let ((path (dotemacs-module-locate-path category module)))
                       (dotemacs-module-set category module :flags flags :path path)
                     (message "Warning: couldn't find the %s %s module" category module)))))))
      dotemacs-modules)
    (dotemacs-initialize-modules)))

(defmacro require! (category module &rest plist)
  "Loads the CATEGORY MODULE module with FLAGS.

CATEGORY is a keyword, MODULE is a symbol and FLAGS are symbols.

  (require! :lang php +lsp)

This is for testing and internal use. This is not the correct way to enable a
module."
  `(let ((dotemacs-modules (or ,dotemacs-modules (dotemacs-modules)))
         (module-path (dotemacs-module-locate-path ,category ',module)))
     (dotemacs-module-set
      ,category ',module
      (let ((plist (dotemacs-module-get ,category ',module)))
        ,(when flags
           `(plist-put plist :flags `,flags))
        (unless (plist-member plist :path)
          (plist-put plist :path ,(dotemacs-module-locate-path category module)))
        plist))
     (if (directory-name-p module-path)
         (condition-case-unless-debug ex
             (progn
               (load! "init" module-path :noerror)
               (load! "config" module-path :noerror))
           ('error
            (lwarn 'dotemacs-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,category ',module
                   (error-message-string ex))))
       (warn 'dotemacs-modules :warning "Couldn't find module '%s %s'"
             ,category ',module))))

(defmacro featurep! (category &optional module flag)
  "Returns t if CATEGORY MODULE is enabled.

If FLAG is provided, returns t if CATEGORY MODULE has FLAG enabled.

  (featurep! :config default)

Module FLAGs are set in your config's `dotemacs!' block, typically in
~/.emacs.d/init.el. Like so:

  :config (default +flag1 -flag2)

CATEGORY and MODULE can be omitted When this macro is used from inside a module
(except your dotemacs dir, which is a special moduel). e.g. (featurep! +flag)"
  (and (cond (flag (memq flag (dotemacs-module-get category module :flags)))
             (module (dotemacs-module-p category module))
             ((let ((module (dotemacs-module-from-path)))
                (unless module
                  (error "(featurep! %s %s %s) couldn't figure out what module it was called from (in %s)"
                         category module flag (file!)))
                (memq category (dotemacs-module-get (car module) (cdr module) :flags)))))
       t))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p dotemacs-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              (let ((body (macroexp-progn body)))
                `(if (featurep ',package)
                     ,body
                   ;; We intentionally avoid `with-eval-after-load' to prevent
                   ;; eager macro expansion from pulling (or failing to pull) in
                   ;; autoloaded macros/packages.
                   (eval-after-load ',package ',body)))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

(provide 'core-modules)
;;; core-modules.el ends here
