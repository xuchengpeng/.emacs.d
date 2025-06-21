;;; init-keybinds.el --- dotemacs keybindings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'transient
  (setq transient-levels-file (expand-file-name "transient/levels.el" dotemacs-cache-dir)
        transient-values-file (expand-file-name "transient/values.el" dotemacs-cache-dir)
        transient-history-file (expand-file-name "transient/history.el" dotemacs-cache-dir)))

(defvar dotemacs-leader-key "M-SPC"
  "The leader prefix key.")

(defvar-keymap dotemacs-leader-map
  :doc "Keymap for `dotemacs-leader-key'."
  "b b" #'switch-to-buffer
  "b k c" #'kill-current-buffer
  "b k m" #'kill-matching-buffers
  "b k M" #'kill-matching-buffers-no-ask
  "b k s" #'kill-some-buffers
  "b n" #'next-buffer
  "b p" #'previous-buffer
  "b r" #'revert-buffer
  "b R" #'revert-buffer-with-coding-system
  "b s" #'save-buffer
  "b S" #'save-some-buffers

  "c a" #'eglot-code-actions
  "c d" #'xref-find-definitions
  "c f" #'eglot-format
  "c i" #'eglot-find-implementation
  "c r" #'xref-find-references
  "c R" #'eglot-rename
  "c s" #'xref-find-apropos

  "f d" #'dired
  "f f" #'consult-fd
  "f r" #'recentf-open-files
  "f w" #'write-file

  "o a" #'org-agenda
  "o c" #'org-capture
  "o d" #'dired-jump
  "o e" #'elfeed
  "o f" #'consult-flymake
  "o g" #'gptel
  "o p" #'org-publish
  "o s" #'+shell

  "q k" #'save-buffers-kill-emacs
  "q q" #'save-buffers-kill-terminal
  "q Q" #'kill-emacs
  "q r" #'restart-emacs

  "s c" #'locate
  "s g" #'consult-grep
  "s i" #'imenu
  "s l" #'consult-line
  "s L" #'consult-line-multi
  "s m" #'bookmark-jump
  "s r" #'consult-ripgrep

  "t f" #'flymake-mode
  "t h" #'hs-toggle-hiding
  "t l" #'display-line-numbers-mode
  "t s" #'flyspell-mode
  "t t" #'modus-themes-toggle
  "t v" #'visible-mode
  "t w" #'visual-line-mode
  "t z" #'toggle-frame-fullscreen)

(which-key-add-keymap-based-replacements dotemacs-leader-map
  "b" "buffer"
  "c" "code"
  "f" "file"
  "o" "open"
  "q" "quit"
  "s" "search"
  "t" "toggle")

(keymap-global-set dotemacs-leader-key dotemacs-leader-map)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
