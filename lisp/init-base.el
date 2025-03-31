;;; init-base.el --- Base configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default cursor-in-non-selected-windows nil
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t
      highlight-nonselected-windows nil
      auto-mode-case-fold nil
      inhibit-compacting-font-caches t
      read-process-output-max (* 4 1024 1024)  ; 4MB
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-input-method nil)
(setq system-time-locale "C")
(if (eq system-type 'windows-nt)
    (progn
      (setq locale-coding-system 'gbk)
      (setq-default process-coding-system-alist
                    '(("[pP][lL][iI][nN][kK]" utf-8 . gbk)
                      ("[cC][mM][dD][pP][rR][oO][xX][yY]" utf-8 . gbk)
                      ("rg" utf-8 . gbk)
                      ("fd" utf-8 . gbk)
                      ("grep" utf-8 . gbk))))
  (progn
    (setq locale-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)))

(defun dotemacs-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defvar +gc-idle-timer nil
  "Idle timer for GC.")

(defun +gc-set-high-threshold ()
  "Set high threshold for GC."
  (setq gc-cons-threshold #x4000000))

(defun +gc-set-low-threshold ()
  "Set low threshold for GC."
  (when (timerp +gc-idle-timer)
    (cancel-timer +gc-idle-timer))
  (setq +gc-idle-timer (run-with-timer 15 nil #'+gc-idle-garbage-collect)))

(defun +gc-idle-garbage-collect ()
  "GC after idle delay."
  (garbage-collect)
  (setq gc-cons-threshold 800000))

(defun +gc-setup ()
  "GC setup."
  (add-hook 'pre-command-hook #'+gc-set-high-threshold)
  (add-hook 'post-command-hook #'+gc-set-low-threshold))

(add-hook 'emacs-startup-hook #'+gc-setup)

(provide 'init-base)
;;; init-base.el ends here
