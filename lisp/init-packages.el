;;; init-packages.el --- Packages configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom dotemacs-package-archives 'melpa
  "Set package archives from which to fetch."
  :type 'symbol)

(setq package-user-dir (expand-file-name "elpa" dotemacs-local-dir))

(let ((proto (if (gnutls-available-p) "https" "http")))
  (pcase dotemacs-package-archives
    ('melpa (setq package-archives `(("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                                     ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                                     ("melpa"  . ,(format "%s://melpa.org/packages/" proto)))))
    ('tuna (setq package-archives `(("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                                    ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                                    ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto)))))))

(package-initialize)

(provide 'init-packages)
;;; init-packages.el ends here
