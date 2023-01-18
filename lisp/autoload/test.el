;;; core/autoload/test.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dotemacs/run-test ()
  "Run tests."
  (interactive)
  (save-buffers-kill-emacs))
