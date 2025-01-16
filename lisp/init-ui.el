;;; init-ui.el --- UI configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 1)

(setq uniquify-buffer-name-style 'forward)

(setq ring-bell-function #'ignore
      visible-bell nil)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs")
      icon-title-format frame-title-format)

(setq window-resize-pixelwise nil
      frame-resize-pixelwise t)

(setq use-short-answers t)

(setq enable-recursive-minibuffers t)

(setq-default major-mode 'fundamental-mode)
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
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

(use-package dired
  :commands (dired dired-jump)
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask
        dired-listing-switches "-alh --group-directories-first"
        image-dired-dir (expand-file-name "image-dired" dotemacs-cache-dir)
        image-dired-db-file (expand-file-name "db.el" image-dired-dir)
        image-dired-gallery-dir (expand-file-name "gallery" image-dired-dir)
        image-dired-temp-image-file (expand-file-name "temp-image" image-dired-dir)
        image-dired-temp-rotate-image-file (expand-file-name "temp-rotate-image" image-dired-dir)
        image-dired-thumb-size 150)
  :config
  (keymap-set dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook
   'dired-mode-hook
   (lambda ()
     (setq-local +modeline-left '(+modeline--window-number +modeline--buffer-default-directory)
                 +modeline-right '(+modeline--major-mode)))))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(use-package dired-aux
  :after (dired)
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(defun +init-fonts ()
  "Set english and chinese fonts."
  (when (display-graphic-p)
    (when dotemacs-font
      (set-face-attribute 'default nil :font dotemacs-font))
    (when dotemacs-cn-font
      (set-fontset-font t 'han dotemacs-cn-font))))

(add-hook 'after-init-hook #'+init-fonts)
(add-hook 'server-after-make-frame-hook #'+init-fonts)

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 4))

(use-package ace-window
  :ensure t
  :commands ace-window
  :init
  (keymap-global-set "<remap> <other-window>" 'ace-window))

(defun +adjust-transparency (frame incr)
  "Adjust the background transparency of FRAME by increment INCR."
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= 0 newalpha) (>= 100 newalpha))
      (set-frame-parameter frame 'alpha-background newalpha))))
(keymap-global-set "C-M-8" (lambda () (interactive) (+adjust-transparency nil -2)))
(keymap-global-set "C-M-9" (lambda () (interactive) (+adjust-transparency nil 2)))
(keymap-global-set "C-M-0" (lambda () (interactive) (set-frame-parameter nil 'alpha-background 100)))

(defun +tab-bar-tab-name-format (tab i)
  "Format a TAB name of tab index I."
  (propertize
   (concat
    " "
    (when tab-bar-tab-hints
      (format "%d " i))
    (alist-get 'name tab)
    " ")
   'face (funcall tab-bar-tab-face-function tab)))

(defun +tab-bar-init ()
  "Tab bar initialize."
  (setq tab-bar-separator "\u200B"
        tab-bar-tab-hints nil
        tab-bar-close-button-show nil
        tab-bar-auto-width nil
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
        tab-bar-tab-name-format-function #'+tab-bar-tab-name-format))

(defun +init-ui ()
  "Initialize UI."
  (pixel-scroll-precision-mode t)

  (load-theme 'modus-operandi :no-confirm)

  (require 'ace-window)
  (require 'init-modeline)
  (+modeline-mode)

  (require 'init-echo-bar)
  (echo-bar-mode)

  (require 'init-dashboard)
  (add-hook
   'dashboard-mode-hook
   (lambda ()
     (setq-local +modeline-left '(+modeline--window-number +modeline--buffer-default-directory)
                 +modeline-right '(+modeline--major-mode))))
  (dashboard-initialize)

  (+tab-bar-init))

(add-hook 'after-init-hook #'+init-ui)

(provide 'init-ui)
;;; init-ui.el ends here
