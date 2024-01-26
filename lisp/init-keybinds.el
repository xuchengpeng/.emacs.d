;;; init-keybinds.el --- dotemacs keybindings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar dotemacs-leader-key "M-SPC"
  "The leader prefix key.")

(defvar dotemacs-localleader-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar-keymap dotemacs-leader-map
  :name "<leader>"
  "b"   '("Buffers" . (keymap))
  "b b" '("Switch buffer" . switch-to-buffer)
  "b k" '("Kill buffer" . kill-this-buffer)
  "b n" '("Next buffer" . next-buffer)
  "b p" '("Previous buffer" . previous-buffer)
  "b r" '("Revert buffer" . revert-buffer)
  "b R" '("Reload encoding" . revert-buffer-with-coding-system)
  "b s" '("Save buffer" . save-buffer)
  "b S" '("Save all buffer" . save-some-buffers)
  "b w" '("Write buffer" . write-file)

  "c"   '("Code" . (keymap))
  "c a" '("Code Action" . eglot-code-actions)
  "c d" '("Find Definition" . xref-find-definitions)
  "c f" '("Format" . eglot-format)
  "c i" '("Find Implementation" . eglot-find-implementation)
  "c r" '("Find References" . xref-find-references)
  "c R" '("Rename" . eglot-rename)
  "c s" '("Workspace Symbols" . consult-eglot-symbols)

  "f"   '("Files" . (keymap))
  "f d" '("Find directory" . dired)
  "f f" '("Find file" . dotemacs-find-file)
  "f g" '("Grep" . dotemacs-search-cwd)
  "f r" '("Recent files" . recentf-open-files)
  "f s" '("Save file" . save-buffer)
  "f w" '("Write file" . write-file)

  "o"   '("Open" . (keymap))
  "o a" '("Org agenda" . org-agenda)
  "o c" '("Org capture" . org-capture)
  "o p" '("Org publish project" . org-publish-project)
  "o d" '("Dired" . dired-jump)
  "o t" '("Terminal" . eshell)

  "q"   '("Quit" . (keymap))
  "q q" '("Quit Emacs" . kill-emacs)
  "q Q" '("Save and quit Emacs" . save-buffers-kill-terminal)
  "q r" '("Restart Emacs" . restart-emacs)

  "s"   '("Search" . (keymap))
  "s b" '("Search buffer" . consult-line)
  "s B" '("Search all open buffers" . consult-line-multi)
  "s d" '("Search current directory" . dotemacs-search-cwd)
  "s D" '("Search other directory" . dotemacs-search-other-cwd)
  "s f" '("Locate file" . locate)
  "s i" '("Jump to symbol" . imenu)
  "s m" '("Jump to bookmark" . bookmark-jump)
  "s p" '("Search project" . consult-ripgrep)
  "s s" '("Search symbol at point" . dotemacs-search-symbol-at-point)

  "t"   '("Toggle" . (keymap))
  "t f" '("Flymake" . flymake-mode)
  "t l" '("Line numbers" . display-line-numbers-mode)
  "t s" '("Spell checker" . flyspell-mode)
  "t t" '("Theme" . dotemacs-load-theme)
  "t v" '("Visible mode" . visible-mode)
  "t w" '("Soft line wrapping" . visual-line-mode)
  "t z" '("Zen mode" . toggle-frame-fullscreen))

(keymap-global-set dotemacs-leader-key dotemacs-leader-map)

(keymap-global-set "C-." 'set-mark-command)
(keymap-global-set "C-x C-." 'pop-global-mark)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
