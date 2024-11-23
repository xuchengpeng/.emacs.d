;;; init-packages.el --- Packages configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(defun +set-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (let ((proto (if (gnutls-available-p) "https" "http")))
    (cond
     ((eq archives 'melpa)
      (setq package-archives `(("gnu"   . ,(format "%s://elpa.gnu.org/packages/" proto))
                               ("melpa" . ,(format "%s://melpa.org/packages/" proto)))))
     ((eq archives 'tuna)
      (setq package-archives `(("gnu"   . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                               ("melpa" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto)))))
     (t
      (error "Unknown archives: '%s'" archives)))))

(unless (eq dotemacs-package-archives 'custom)
  (+set-package-archives dotemacs-package-archives))

(setq load-prefer-newer t
      package-enable-at-startup nil
      package-user-dir (expand-file-name "elpa" dotemacs-local-dir)
      package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(package-initialize)

(provide 'init-packages)
;;; init-packages.el ends here
