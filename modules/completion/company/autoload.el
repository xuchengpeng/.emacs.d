;;; completion/company/autoload.el -*- lexical-binding: t; -*-

(defvar +company/enable-yas dotemacs-company-enable-yas
  "Enable yasnippet for all backends.")

;;;###autoload
(defun +company/backend-with-yas (backend)
  (if (or (not +company/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (ignore-errors
          (/= (point)
              (cdr (bounds-of-thing-at-point 'symbol))))
    (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))
