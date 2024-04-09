;;; init-python.el -- Python. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun dotemacs-python-format ()
  "Format current buffer with `black'."
  (interactive)
  (unless (executable-find "black")
    (error "`black' not found"))
  (dotemacs-format-buffer "black" "-q"))

(provide 'init-python)
;;; init-python.el ends here
