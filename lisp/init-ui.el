;;; init-ui.el --- UI configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(modus-themes which-key ace-window))

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . 2))
      mouse-wheel-scroll-amount-horizontal 2)

(setq uniquify-buffer-name-style 'forward)

;;;###autoload
(defun dotemacs-ring-bell-fn ()
  "Blink the mode-line."
  (let ((buf (current-buffer)))
    (invert-face 'mode-line)
    (run-with-timer 0.05 nil
                    (lambda ()
                      (with-current-buffer buf
                        (invert-face 'mode-line))))))

(setq ring-bell-function 'dotemacs-ring-bell-fn
      visible-bell t)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

(setq window-resize-pixelwise nil
      frame-resize-pixelwise t)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'fundamental-mode)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package winner
  :hook (window-configuration-change . winner-mode)
  :config
  (setq winner-boring-buffers (append winner-boring-buffers
        '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
          "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
          "*esh command on file*"))))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)
         ((eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(defun dotemacs-init-fonts ()
  "Set english and chinese fonts."
  (when (display-graphic-p)
    (when dotemacs-font
      (set-face-attribute 'default nil :font dotemacs-font))
    (when dotemacs-cn-font
      (set-fontset-font t 'han dotemacs-cn-font))))

(add-hook 'after-init-hook #'dotemacs-init-fonts)
(add-hook 'server-after-make-frame-hook #'dotemacs-init-fonts)

;;;###autoload
(defun dotemacs-init-theme ()
  "Init theme."
  (require 'modus-themes)
  (load-theme 'modus-operandi :no-confirm)

  (push (expand-file-name "lisp/tokyonight-themes" dotemacs-dir) load-path)
  (require 'tokyonight-themes))

(defun dotemacs-load-theme ()
  "Load theme."
  (interactive)
  (let ((choice (completing-read
                  "Select theme: "
                  '("tokyonight-storm" "tokyonight-night" "tokyonight-moon" "tokyonight-day"
                    "modus-operandi" "modus-vivendi"))))
    (consult-theme (intern choice))))

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 4))

(use-package ace-window
  :commands ace-window
  :init
  (keymap-global-set "<remap> <other-window>" 'ace-window))

(defun dotemacs-adjust-transparency (frame incr)
  "Adjust the background transparency of FRAME by increment INCR."
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= 0 newalpha) (>= 100 newalpha))
      (set-frame-parameter frame 'alpha-background newalpha))))
(keymap-global-set "C-M-8" (lambda () (interactive) (dotemacs-adjust-transparency nil -2)))
(keymap-global-set "C-M-9" (lambda () (interactive) (dotemacs-adjust-transparency nil 2)))
(keymap-global-set "C-M-0" (lambda () (interactive) (set-frame-parameter nil 'alpha-background 100)))

(add-hook
 'after-init-hook
 (lambda ()
   (when (fboundp 'pixel-scroll-precision-mode)
     (pixel-scroll-precision-mode t))

   (dotemacs-init-theme)
   (require 'ace-window)
   ;; show tab-name in mode-line
   (setq tab-bar-show nil)
   (require 'init-modeline)
   (dotemacs-modeline-mode)))

(add-hook
 'window-setup-hook
 (lambda ()
   (require 'init-dashboard)
   (dotemacs-dashboard-init)))

(provide 'init-ui)
;;; init-ui.el ends here
