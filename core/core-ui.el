;;; core-ui.el --- Initialize core ui configurations. -*- lexical-binding: t; -*-

(defvar dotemacs-theme nil
  "A symbol representing the color theme to load.")

(defvar dotemacs-font nil
  "The default font to use.

Expects either a `font-spec', or a font string.

Examples:
  (setq dotemacs-font (font-spec :family \"Fira Mono\" :size 12))
  (setq dotemacs-font \"Terminus (TTF):pixelsize=12:antialias=off\")")

(defvar dotemacs-cn-font nil
  "The chinese font to use.")

;;
;;; Custom hooks

(defvar dotemacs-load-theme-hook nil
  "Hook run when the theme is initialized.")

(defvar dotemacs-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defvar dotemacs-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defvar dotemacs-inhibit-switch-buffer-hooks nil
  "Letvar for inhibiting `dotemacs-switch-buffer-hook'. Do not set this directly.")

(defun dotemacs-run-switch-buffer-hooks-a (orig-fn buffer-or-name &rest args)
  (if (or dotemacs-inhibit-switch-buffer-hooks
          (and buffer-or-name
               (eq (current-buffer)
                   (get-buffer buffer-or-name)))
          (and (eq orig-fn #'switch-to-buffer) (car args)))
      (apply orig-fn buffer-or-name args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (dotemacs-inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn buffer-or-name args))
        (with-current-buffer (if (windowp buffer)
                                 (window-buffer buffer)
                               buffer)
          (run-hooks 'dotemacs-switch-buffer-hook))
        buffer))))

(defun dotemacs-run-switch-to-next-prev-buffer-hooks-a (orig-fn &rest args)
  (if dotemacs-inhibit-switch-buffer-hooks
      (apply orig-fn args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (dotemacs-inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn args))
        (with-current-buffer buffer
          (run-hooks 'dotemacs-switch-buffer-hook))
        buffer))))

;;
;;; General UX

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . 2))
      mouse-wheel-scroll-amount-horizontal 2)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;;
;;; Cursor

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

(setq window-resize-pixelwise nil
      frame-resize-pixelwise t)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; _while_ we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;
;;; line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

;;
;; Built-in packages

;; undo/redo changes to Emacs' window layout
(use-package winner
  :hook (window-configuration-change . winner-mode)
  :config
  (winner-mode +1)
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))

;; Highlight the current line
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode))

;; highlight matching delimiters
(use-package paren
  :hook (find-file . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

;;
;; Theme & font

(defun dotemacs-set-font-fn()
  "Set english and chinese fonts."
  (when dotemacs-font
    (set-face-attribute 'default nil :font dotemacs-font))
  (when dotemacs-cn-font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset dotemacs-cn-font))))

(defun dotemacs-init-fonts-h ()
  "Set the font."
  (add-to-list 'after-make-frame-functions
               (lambda (new-frame)
                 (select-frame new-frame)
                 (when (display-graphic-p)
                   (dotemacs-set-font-fn))))
  (when (display-graphic-p)
    (dotemacs-set-font-fn)))

(defadvice! dotemacs--run-load-theme-hooks-a (theme &optional _no-confirm no-enable)
  "Set up `dotemacs-load-theme-hook' to run after `load-theme' is called."
  :after-while #'load-theme
  (unless no-enable
    (setq dotemacs-theme theme)
    (run-hooks 'dotemacs-load-theme-hook)))

(defadvice! dotemacs--disable-enabled-themes-a (theme &optional _no-confirm no-enable)
  "Disable previously enabled themes before loading a new one.
Otherwise, themes can conflict with each other."
  :after-while #'load-theme
  (unless no-enable
    (mapc #'disable-theme (remq theme custom-enabled-themes))))

(defun dotemacs-init-theme-h ()
  "Set the theme."
  (when (and dotemacs-theme (not (memq dotemacs-theme custom-enabled-themes)))
    (load-theme dotemacs-theme t)))

;; fonts
(add-hook 'dotemacs-init-ui-hook #'dotemacs-init-fonts-h)
;; themes
(add-hook 'dotemacs-init-ui-hook #'dotemacs-init-theme-h)

;;
;; Bootstrap

(defun dotemacs-init-ui-h ()
  "Initialize ui."
  (dotemacs-run-hooks 'dotemacs-init-ui-hook)

  (dolist (fn '(switch-to-next-buffer switch-to-prev-buffer))
    (advice-add fn :around #'dotemacs-run-switch-to-next-prev-buffer-hooks-a))
  (dolist (fn '(switch-to-buffer display-buffer))
    (advice-add fn :around #'dotemacs-run-switch-buffer-hooks-a)))

(add-hook 'window-setup-hook #'dotemacs-init-ui-h)

(provide 'core-ui)
;;; core-ui.el ends here
