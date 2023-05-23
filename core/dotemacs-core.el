;;; dotemacs-core.el --- Emacs Core functions. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Core configuration.
;;

;;; Code:

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq idle-update-delay 1.0)  ; default is 0.5
(setq auto-mode-case-fold nil)
(setq inhibit-compacting-font-caches t)
(setq read-process-output-max (* 64 1024))  ; 64kb
(setq ffap-machine-p-known 'reject)
(setq redisplay-skip-fontification-on-input t)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq system-time-locale "C")
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