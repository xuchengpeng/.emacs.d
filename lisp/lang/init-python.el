;;; init-python.el -- Python. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun dotemacs-python-format ()
  "Format current buffer with `black'."
  (interactive)
  (unless (executable-find "black")
    (error "`black' not found"))
  (if buffer-file-name
      (progn
        (save-buffer)
        (shell-command
         (format "black -q %s" buffer-file-name)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "black - -q")))

(provide 'init-python)
;;; init-python.el ends here
