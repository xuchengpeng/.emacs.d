;;; completion/company/autoload.el -*- lexical-binding: t; -*-

(defvar company-mode/enable-yas dotemacs-company-enable-yas
  "Enable yasnippet for all backends.")

;;;###autoload
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
