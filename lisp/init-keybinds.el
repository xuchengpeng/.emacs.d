;;; init-keybinds.el --- dotemacs keybindings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'transient
  (setq transient-levels-file (expand-file-name "transient/levels.el" dotemacs-cache-dir)
        transient-values-file (expand-file-name "transient/values.el" dotemacs-cache-dir)
        transient-history-file (expand-file-name "transient/history.el" dotemacs-cache-dir)))

(defvar dotemacs-leader-key "M-SPC"
  "The leader prefix key.")

(defvar dotemacs-localleader-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar-keymap dotemacs-leader-map
  :doc "Keymap for `dotemacs-leader-key'."
  "b"   '("Buffers" . (keymap))
  "b b" '("Switch" . switch-to-buffer)
  "b k" '("Kill Current" . kill-current-buffer)
  "b K" '("Kill" . kill-buffer)
  "b n" '("Next" . next-buffer)
  "b p" '("Previous" . previous-buffer)
  "b r" '("Revert" . revert-buffer)
  "b R" '("Reload Encoding" . revert-buffer-with-coding-system)
  "b s" '("Save" . save-buffer)
  "b S" '("Save All" . save-some-buffers)
  "b w" '("Write File" . write-file)

  "f"   '("Files" . (keymap))
  "f d" '("Find Directory" . dired)
  "f f" '("Find File" . +find-file)
  "f r" '("Recent Files" . recentf-open-files)
  "f s" '("Save Buffer" . save-buffer)
  "f w" '("Write File" . write-file)

  "l"   '("LSP" . (keymap))
  "l a" '("Code Action" . eglot-code-actions)
  "l d" '("Find Definition" . xref-find-definitions)
  "l f" '("Format" . eglot-format)
  "l i" '("Find Implementation" . eglot-find-implementation)
  "l r" '("Find References" . xref-find-references)
  "l R" '("Rename" . eglot-rename)
  "l s" '("Workspace Symbols" . xref-find-apropos)
  "l x" '("Diagnostic" . consult-flymake)

  "o"   '("Open" . (keymap))
  "o a" '("Org Agenda" . org-agenda)
  "o c" '("Org Capture" . org-capture)
  "o f" '("Flymake Diagnostic" . consult-flymake)
  "o p" '("Org Publish Project" . org-publish-project)
  "o d" '("Dired" . dired-jump)
  "o t" '("Terminal" . +shell)

  "q"   '("Quit" . (keymap))
  "q q" '("Quit Emacs" . kill-emacs)
  "q Q" '("Save Quit Emacs" . save-buffers-kill-terminal)
  "q r" '("Restart Emacs" . restart-emacs)

  "s"   '("Search" . (keymap))
  "s b" '("Buffer" . consult-line)
  "s B" '("Multiple Buffers" . consult-line-multi)
  "s d" '("Current Directory" . +search-cwd)
  "s D" '("Other Directory" . +search-other-cwd)
  "s f" '("Locate" . locate)
  "s g" '("Grep" . consult-grep)
  "s i" '("Imenu" . imenu)
  "s m" '("Bookmark" . bookmark-jump)
  "s r" '("Ripgrep" . consult-ripgrep)
  "s s" '("Symbol" . (lambda () (interactive) (consult-ripgrep nil (thing-at-point 'symbol))))

  "t"   '("Toggle" . (keymap))
  "t f" '("Flymake" . flymake-mode)
  "t l" '("Line Numbers" . display-line-numbers-mode)
  "t s" '("Flyspell" . flyspell-mode)
  "t t" '("Theme" . modus-themes-toggle)
  "t v" '("Visible Mode" . visible-mode)
  "t w" '("Visual Line Mode" . visual-line-mode)
  "t z" '("Zen Mode" . toggle-frame-fullscreen))

(keymap-global-set dotemacs-leader-key dotemacs-leader-map)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
