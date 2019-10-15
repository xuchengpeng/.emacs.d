;;; emacs/eshell/config.el -*- lexical-binding: t; -*-

(defvar +eshell-config-dir
  (expand-file-name "eshell/" dotemacs-local-dir)
  "Where to store eshell configuration files, as opposed to
`eshell-directory-name', which is where Doom will store temporary/data files.")

(defvar +eshell-aliases
  '(("q"  "exit")           ; built-in
    ("f"  "find-file $1")
    ("bd" "eshell-up $1")
    ("rg" "rg --color=always --smart-case --line-number --no-heading $*")
    ("ag" "ag --smart-case --color --line-numbers --nogroup $*")
    ("l"  "ls -lh")
    ("ll" "ls -lah")
    ("clear" "clear-scrollback")) ; more sensible than default
  "An alist of default eshell aliases, meant to emulate useful shell utilities,
like fasd and bd. Note that you may overwrite these in your
`eshell-aliases-file'. This is here to provide an alternative, elisp-centric way
to define your aliases.

You should use `set-eshell-alias!' to change this.")

(defvar eshell-directory-name (concat dotemacs-cache-dir "eshell"))

;; These files are exceptions, because they may contain configuration
(defvar eshell-aliases-file (concat +eshell-config-dir "alias"))
(defvar eshell-rc-script    (concat +eshell-config-dir "profile"))
(defvar eshell-login-script (concat +eshell-config-dir "login"))

(defvar +eshell--default-aliases nil)

(use-package eshell
  :commands (eshell eshell-mode)
  :config
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        ;; don't record command in history if prefixed with whitespace
        ;; TODO Use `eshell-input-filter-initial-space' when Emacs 25 support is dropped
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-default-prompt-fn
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)
    
    (defun +eshell-init-keymap-h ()
      "Init eshell keymap."
      (define-key! eshell-mode-map
        "C-s"   '+eshell/search-history))
    (add-hook 'eshell-first-time-mode-hook #'+eshell-init-keymap-h)
    
    ;; Consider eshell buffers real
    (add-hook 'eshell-mode-hook #'dotemacs-mark-buffer-as-real-h)
    
    ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file'
    ;; or configure `+eshell-aliases' via elisp.
    (advice-add #'eshell-write-aliases-list :override #'ignore)
    
    (add-hook! 'eshell-alias-load-hook
      (defun +eshell-init-aliases-h ()
        (setq +eshell--default-aliases eshell-command-aliases-list
              eshell-command-aliases-list
              (append eshell-command-aliases-list
                      +eshell-aliases)))))

(use-package shell-pop
  :commands (shell-pop)
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))
