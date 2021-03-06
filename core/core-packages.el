;;; core-packages.el --- Initialize core packages configurations. -*- lexical-binding: t; -*-

(defvar dotemacs-core-packages '(straight use-package bind-key general)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar dotemacs-packages ()
  "A list of enabled packages.")

(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-user-dir (expand-file-name "elpa" dotemacs-packages-dir)
      package-gnupghome-dir (expand-file-name "gnupg" dotemacs-packages-dir)
      load-prefer-newer noninteractive
      use-package-verbose dotemacs-debug-p
      use-package-compute-statistics dotemacs-debug-p
      use-package-minimum-reported-time (if dotemacs-debug-p 0 0.1)
      use-package-expand-minimally (not dotemacs-debug-p))

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

;; Don't save `package-selected-packages' to `custom-file'
(defadvice! dotemacs--package-inhibit-custom-file-a (&optional value)
  :override #'package--save-selected-packages
  (if value (setq package-selected-packages value)))

;;; straight
(setq straight-base-dir dotemacs-local-dir
      straight-repository-branch "master"
      straight-vc-git-default-clone-depth 1
      straight-recipes-gnu-elpa-use-mirror t
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
           (format "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
                   straight-repository-branch)
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;
;;; Functions

(defun dotemacs-initialize-packages (&optional force-p)
  "Initial core packages."
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (dotemacs-log "Initializing package.el")
    (require 'package)
    (package-initialize))
  
  (dotemacs-log "Initializing straight")
  (dotemacs-ensure-straight)
  (require 'straight)

  (straight-register-package
   `(straight :type git :host github
              :repo ,(format "%s/straight.el" straight-repository-user)
              :files ("straight*.el")
              :branch ,straight-repository-branch
              :no-byte-compile t))
  (dotemacs-log "Initializing dotemacs-packages")
  (mapc #'straight-use-package dotemacs-core-packages)
  
  (unless dotemacs-interactive-p
    (add-hook 'kill-emacs-hook #'dotemacs--finalize-straight)))

(defun dotemacs-install-packages (packages-list)
  "Install packages defined by PACKAGES-LIST."
  (mapc #'straight-use-package packages-list))

;;
;;; Module package macros

(defmacro package! (package)
  "Add PACKAGE to ‘dotemacs-packages’."
  `(add-to-list 'dotemacs-packages ',package t))

(defmacro packages! (&rest packages)
  "Add packages in PACKAGES to ‘dotemacs-packages’.

Can take multiple packages.
e.g. (packages! evil evil-surround)"
  `(dolist (package ',packages)
     (add-to-list 'dotemacs-packages package t)))

;;
;;; Initialize packages
(dotemacs-initialize-packages)

(provide 'core-packages)
;;; core-packages.el ends here
