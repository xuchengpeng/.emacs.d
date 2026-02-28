;;; init-base.el --- Base configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

(set-language-environment "UTF-8")
(if (eq system-type 'windows-nt)
    (setq-default buffer-file-coding-system 'utf-8-unix
                  process-coding-system-alist
                  '(("cmdproxy" utf-8 . gbk)
                    ("rg" utf-8 . gbk)
                    ("fd" utf-8 . gbk)
                    ("grep" utf-8 . gbk)))
  (set-selection-coding-system 'utf-8))
;; `set-language-environment' also sets `default-input-method'.
(setq default-input-method nil)
(setq system-time-locale "C")

(defun dotemacs-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(provide 'init-base)
;;; init-base.el ends here
