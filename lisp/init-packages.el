;;; init-packages.el --- Packages configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom dotemacs-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Tuna" tuna)
          (const :tag "Custom" custom)))

(setq package-user-dir (expand-file-name "elpa" dotemacs-local-dir))

(defun +set-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (let ((proto (if (gnutls-available-p) "https" "http")))
    (cond
     ((eq archives 'melpa)
      (setq package-archives `(("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                               ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                               ("melpa"  . ,(format "%s://melpa.org/packages/" proto)))))
     ((eq archives 'tuna)
      (setq package-archives `(("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                               ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                               ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto)))))
     (t
      (error "Unknown archives: '%s'" archives)))))

(unless (eq dotemacs-package-archives 'custom)
  (+set-package-archives dotemacs-package-archives))

(package-initialize)

(provide 'init-packages)
;;; init-packages.el ends here
