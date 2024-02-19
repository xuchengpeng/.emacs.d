;;; init-keybinds.el --- dotemacs keybindings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar dotemacs-leader-key "M-SPC"
  "The leader prefix key.")

(defvar dotemacs-localleader-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar-keymap dotemacs-leader-map
  :doc "Keymap for `dotemacs-leader-key'."
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

  "f"   '("Files" . (keymap))
  "f d" '("Find directory" . dired)
  "f f" '("Find file" . dotemacs-find-file)
  "f g" '("Grep" . dotemacs-search-cwd)
  "f r" '("Recent files" . recentf-open-files)
  "f s" '("Save file" . save-buffer)
  "f w" '("Write file" . write-file)

  "l"   '("LSP" . (keymap))
  "l a" '("Code Action" . eglot-code-actions)
  "l d" '("Find Definition" . xref-find-definitions)
  "l f" '("Format" . eglot-format)
  "l i" '("Find Implementation" . eglot-find-implementation)
  "l r" '("Find References" . xref-find-references)
  "l R" '("Rename" . eglot-rename)
  "l s" '("Workspace Symbols" . consult-eglot-symbols)
  "l x" '("Diagnostic" . flymake-show-buffer-diagnostics)

  "o"   '("Open" . (keymap))
  "o a" '("Org agenda" . org-agenda)
  "o c" '("Org capture" . org-capture)
  "o f" '("Flymake diagnostic" . consult-flymake)
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
  "t z" '("Zen mode" . toggle-frame-fullscreen)

  "w"   '("Workspaces" . (keymap))
  "w [" '("Previous" . tab-previous)
  "w ]" '("Next" . tab-next)
  "w b" '("Switch Buffer Other Tab" . switch-to-buffer-other-tab)
  "w c" '("Close" . tab-close)
  "w C" '("Close Other" . tab-close-other)
  "w d" '("Dired Other Tab" . dired-other-tab)
  "w f" '("Find File Other Tab" . find-file-other-tab)
  "w g" '("Group" . tab-group)
  "w n" '("New" . tab-new)
  "w p" '("Project Other Tab" . project-other-tab-command)
  "w r" '("Rename" . tab-rename)
  "w s" '("Switch" . tab-switch))

(keymap-global-set dotemacs-leader-key dotemacs-leader-map)

(keymap-global-set "C-." 'set-mark-command)
(keymap-global-set "C-x C-." 'pop-global-mark)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
