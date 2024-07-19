;;; init-utils.el --- utils. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq idle-update-delay 1.0)  ; default is 0.5
(setq auto-mode-case-fold nil)
(setq inhibit-compacting-font-caches t)
(setq read-process-output-max (* 4 1024 1024))  ; 4MB
(setq ffap-machine-p-known 'reject)
(setq redisplay-skip-fontification-on-input t)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq system-time-locale "C")
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(setq-default buffer-file-coding-system 'utf-8-unix)

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
Same as `replace-string' RET `C-q' `C-m' RET RET, or `query-replace'."
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

(defun dotemacs-treesit-available-p ()
  "Check if treesit available."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(provide 'init-utils)
;;; init-utils.el ends here
