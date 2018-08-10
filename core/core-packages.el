
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

(defun dotemacs-keyword-to-name-str (keyword)
  "Remove the colon in KEYWORD symbol and turn it into string.

i.e. :keyword to \"keyword\"."
  (replace-regexp-in-string "^:" "" (symbol-name keyword)))

(defmacro dotemacs! (&rest modules-list)
  "Declare stars in STAR-LIST.
Separate stars with sub-directories' name.
Basically adding path to star to `moon-star-path-list'.

Example: (moon| :feature evil :ui custom) for star/feature/evil
and star/ui/custom.
If called multiple times, the stars declared first will be
in the front of moon-star-list.

`moon|' can be used in star's `package.el',
but try to only depend on stars in `:basic' sub directory.

Because a star's dependencies' dependency will not be added automatically.
If your star's dependency star denpend of some other star,
that star will not be included by lunarymacs framework
when loading and installing packages.

In a word, denpend of stars that don't depend on other stars!"
  (dolist (module modules-list)
    (cond ((keywordp module) (setq mode module))
          ((not      mode) (error "No sub-folder specified in `moon|' for %s" module))
          (t               (let ((module-path (format "%s%s/%s/" dotemacs-modules-dir (dotemacs-keyword-to-name-str mode) module)))
                             (add-to-list 'doteamcs-modules-path-list module-path t))))))

(defmacro package! (&rest packages-list)
  "Add packages in PACKAGE-LIST to ¡®dotemacs-packages¡¯.

Can take multiple packages.
e.g. (package! evil evil-surround)"
  `(dolist (package ',packages-list)
     (add-to-list 'dotemacs-packages package)))

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

(defun dotemacs-modules-load-package (path-list)
  "Load packages.el in each star in PATH-LIST."
  (dolist (m-path path-list)
    (let ((path (concat m-path "packages.el")))
      (when (file-exists-p path)
        (load path))))
  (dotemacs-install-packages dotemacs-packages))

(defun dotemacs-modules-load-config (path-list)
  "Load config.el in each star in PATH-LIST."
  (dolist (m-path path-list)
    (let ((path (concat m-path "config.el")))
      (if (file-exists-p path)
          (load path)
        (message (format "%s does not exist!" path)))
      )))

(defun dotemacs-initialize-modules ()
  (dotemacs-modules-load-package doteamcs-modules-path-list)
  (dotemacs-modules-load-config doteamcs-modules-path-list))

(provide 'core-packages)
