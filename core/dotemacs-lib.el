;;; dotemacs-lib.el --- Libraries. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dotemacs-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defun dotemacs-delete-carrage-returns ()
  "Delete ^M of current buffer.
Same as M-x replace-string RET C-q C-m RET RET, or query-replace."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

(defun dotemacs-dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun dotemacs-unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(provide 'dotemacs-lib)
;;; dotemacs-lib.el ends here
