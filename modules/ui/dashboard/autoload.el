;;; ui/dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dashboard|reload (frame)
  "Force recreation of the dashboard buffer in the current window,
of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (+dashboard|init t)))
