;;; dotemacs-packages.el --- Packages configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(defun dotemacs-set-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (let ((proto (if (gnutls-available-p) "https" "http")))
    (cond
     ((eq archives 'melpa)
      (setq package-archives `(("gnu"   . ,(format "%s://elpa.gnu.org/packages/" proto))
                               ("melpa" . ,(format "%s://melpa.org/packages/" proto)))))
     ((eq archives 'tuna)
      (setq package-archives `(("gnu"   . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                               ("melpa" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto)))))
     ((eq archives 'custom)
      (setq package-archives dotemacs-custom-package-archives))
     (t
      (error "Unknown archives: '%s'" archives)))))

(dotemacs-set-package-archives dotemacs-package-archives)

(setq load-prefer-newer t
      package-enable-at-startup nil
      package-user-dir (concat dotemacs-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(package-initialize)

(defun dotemacs-require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (dotemacs-require-package package min-version t)))
        (package-installed-p package min-version))))

(defun dotemacs-require-packages (packages)
  "Install PACKAGES."
  (mapc #'dotemacs-require-package packages))

(dotemacs-require-package 'use-package)

(provide 'dotemacs-packages)
;;; dotemacs-packages.el ends here
