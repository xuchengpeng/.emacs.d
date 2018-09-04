;;; ui/dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dashboard/reload ()
  "Force recreation of the dashboard buffer."
  (interactive)
  (+dashboard/init-dashboard t))
