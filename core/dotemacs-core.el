;;; dotemacs-core.el --- Emacs Core functions.

;;; Commentary:
;;
;; Core configuration.
;;

;;; Code:

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(set-language-environment "UTF-8")
(setq default-input-method nil)
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))
(setq-default buffer-file-coding-system 'utf-8-unix)

(defun dotemacs-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defun dotemacs-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(provide 'dotemacs-core)
;;; dotemacs-core.el ends here