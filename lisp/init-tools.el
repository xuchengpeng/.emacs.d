;;; init-tools.el -- Tools configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'windows-nt)
  (defvar eshell-directory-name (expand-file-name "eshell" dotemacs-cache-dir))
  (defvar eshell-aliases-file (expand-file-name "eshell/aliases" dotemacs-local-dir))
  (defvar eshell-rc-script    (expand-file-name "eshell/profile" dotemacs-local-dir))
  (defvar eshell-login-script (expand-file-name "eshell/login" dotemacs-local-dir))

  (defvar +eshell-aliases
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
    (add-hook
     'eshell-mode-hook
     (lambda ()
       (setq-local hscroll-margin 0
                   +modeline-left '(+modeline--window-number +modeline--buffer-default-directory)
                   +modeline-right '(+modeline--major-mode))
       (visual-line-mode)))
    (with-eval-after-load 'em-alias
      (setq eshell-command-aliases-list (append eshell-command-aliases-list +eshell-aliases)))))

(unless (eq system-type 'windows-nt)
  (use-package vterm
    :ensure t
    :commands (vterm vterm-other-window)
    :config
    (add-hook
     'vterm-mode-hook
     (lambda ()
       (setq-local +modeline-left '(+modeline--window-number +modeline--buffer-default-directory)
                   +modeline-right '(+modeline--major-mode))))))

(defun +shell ()
  "Launch shell."
  (interactive)
  (if (eq system-type 'windows-nt)
      (eshell)
    (vterm)))

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-rewrite)
  :config
  (setq gptel-model 'gemini-2.0-flash
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (lambda () (getenv "GPTEL_GEMINI_KEY"))
                        :stream nil)))

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" dotemacs-local-dir)
        elfeed-feeds '("https://planet.emacslife.com/atom.xml")
        elfeed-search-filter "@1-month-ago "
        url-queue-timeout 30))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :init
  (defun +denote-find ()
    "Find in denote directory."
    (interactive)
    (require 'denote)
    (consult-fd denote-directory nil))

  (defun +denote-grep ()
    "Grep in denote directory."
    (interactive)
    (require 'denote)
    (consult-ripgrep denote-directory nil))

  (defvar-keymap +denote-map
    :doc "Denote map."
    "n" #'denote
    "s" #'denote-subdirectory
    "t" #'denote-type
    "d" #'denote-sort-dired
    "l" #'denote-link
    "L" #'denote-add-links
    "b" #'denote-backlinks
    "r" #'denote-rename-file
    "R" #'denote-rename-file-using-front-matter
    "f" #'+denote-find
    "g" #'+denote-grep)
  (keymap-global-set "C-c n" +denote-map)
  :config
  (setq denote-directory dotemacs-note-dir)
  (denote-rename-buffer-mode 1))

(provide 'init-tools)
;;; init-tools.el ends here
