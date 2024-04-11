;;; init-eshell.el --- eshell. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar eshell-directory-name (expand-file-name "eshell" dotemacs-cache-dir))
(defvar eshell-aliases-file (expand-file-name "eshell/aliases" dotemacs-local-dir))
(defvar eshell-rc-script    (expand-file-name "eshell/profile" dotemacs-local-dir))
(defvar eshell-login-script (expand-file-name "eshell/login" dotemacs-local-dir))

(defvar dotemacs-eshell-aliases
  '(("q" "exit")
    ("f" "find-file $1")
    ("d" "dired $1")
    ("l" "ls -lh $*")
    ("ll" "ls -lah $*")
    ("clear" "clear-scrollback"))
  "An alist of default eshell aliases.")

(use-package eshell
  :commands eshell
  :config
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local hscroll-margin 0
                          dotemacs-modeline-left '(dotemacs-modeline--window-number
                                                   dotemacs-modeline--buffer-default-directory)
                          dotemacs-modeline-right '(dotemacs-modeline--major-mode))
              (visual-line-mode)))
  (with-eval-after-load 'em-alias
    (setq eshell-command-aliases-list (append eshell-command-aliases-list dotemacs-eshell-aliases))))

(provide 'init-eshell)
;;; init-eshell.el ends here
