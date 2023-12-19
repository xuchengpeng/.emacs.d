;;; dotemacs-keybinds.el --- dotemacs keybindings. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; dotemacs keybinds.
;;

;;; Code:

(defvar dotemacs-leader-map (make-sparse-keymap)
  "Leader map.")
(global-set-key (kbd "M-SPC") dotemacs-leader-map)

(defvar dotemacs-major-mode-map-alist nil
  "Alist of major modes and mode map.")

(defun dotemacs-set-major-mode-map ()
  "Set mode map base on major-mode."
  (catch 'found
    (dolist (x dotemacs-major-mode-map-alist)
      (when (derived-mode-p (car x))
        (local-set-key (kbd "M-SPC m") (symbol-value (cdr x)))
        (throw 'found x)))))

(add-hook 'after-change-major-mode-hook #'dotemacs-set-major-mode-map)

(define-key dotemacs-leader-map "b"  '("Buffers" . (keymap)))
(define-key dotemacs-leader-map "bb" '("Switch buffer" . switch-to-buffer))
(define-key dotemacs-leader-map "bk" '("Kill buffer" . kill-this-buffer))
(define-key dotemacs-leader-map "bn" '("Next buffer" . next-buffer))
(define-key dotemacs-leader-map "bp" '("Previous buffer" . previous-buffer))
(define-key dotemacs-leader-map "br" '("Revert buffer" . revert-buffer))
(define-key dotemacs-leader-map "bR" '("Reload encoding" . revert-buffer-with-coding-system))
(define-key dotemacs-leader-map "bs" '("Save buffer" . save-buffer))
(define-key dotemacs-leader-map "bS" '("Save all buffer" . save-some-buffers))
(define-key dotemacs-leader-map "bw" '("Write buffer" . write-file))

(define-key dotemacs-leader-map "f"  '("Files" . (keymap)))
(define-key dotemacs-leader-map "fd" '("Find directory" . dired))
(define-key dotemacs-leader-map "ff" '("Find file" . dotemacs-find-file))
(define-key dotemacs-leader-map "fg" '("Grep" . dotemacs-search-cwd))
(define-key dotemacs-leader-map "fr" '("Recent files" . recentf-open-files))
(define-key dotemacs-leader-map "fs" '("Save file" . save-buffer))
(define-key dotemacs-leader-map "fw" '("Write file" . write-file))

(define-key dotemacs-leader-map "l"  '("Lsp" . (keymap)))
(define-key dotemacs-leader-map "la" '("Code Action" . lsp-execute-code-action))
(define-key dotemacs-leader-map "ld" '("Find Definition" . lsp-find-definition))
(define-key dotemacs-leader-map "lf" '("Format" . lsp-format-buffer))
(define-key dotemacs-leader-map "li" '("Find Implementation" . lsp-find-implementation))
(define-key dotemacs-leader-map "lr" '("Find References" . lsp-find-references))
(define-key dotemacs-leader-map "lR" '("Rename" . lsp-rename))
(define-key dotemacs-leader-map "ls" '("File Symbols" . consult-lsp-file-symbols))
(define-key dotemacs-leader-map "lS" '("Workspace Symbols" . consult-lsp-symbols))

(define-key dotemacs-leader-map "o"  '("Open" . (keymap)))
(define-key dotemacs-leader-map "oa" '("Org agenda" . org-agenda))
(define-key dotemacs-leader-map "oc" '("Org capture" . org-capture))
(define-key dotemacs-leader-map "op" '("Org publish project" . org-publish-project))
(define-key dotemacs-leader-map "od" '("Dired" . dired-jump))
(define-key dotemacs-leader-map "ot" '("Terminal" . eshell))

(define-key dotemacs-leader-map "p"  '("Project" . (keymap)))
(define-key dotemacs-leader-map "pf" '("Find file in project" . projectile-find-file))
(define-key dotemacs-leader-map "po" '("Find other project file" . projectile-find-other-file))
(define-key dotemacs-leader-map "pr" '("Recent project files" . projectile-recentf))
(define-key dotemacs-leader-map "ps" '("Search project" . consult-ripgrep))
(define-key dotemacs-leader-map "pp" '("Switch project" . projectile-switch-project))

(define-key dotemacs-leader-map "q"  '("Quit" . (keymap)))
(define-key dotemacs-leader-map "qq" '("Quit Emacs" . kill-emacs))
(define-key dotemacs-leader-map "qQ" '("Save and quit Emacs" . save-buffers-kill-terminal))

(define-key dotemacs-leader-map "s"  '("Search" . (keymap)))
(define-key dotemacs-leader-map "sb" '("Search buffer" . consult-line))
(define-key dotemacs-leader-map "sB" '("Search all open buffers" . consult-line-multi))
(define-key dotemacs-leader-map "sd" '("Search current directory" . dotemacs-search-cwd))
(define-key dotemacs-leader-map "sD" '("Search other directory" . dotemacs-search-other-cwd))
(define-key dotemacs-leader-map "sf" '("Locate file" . locate))
(define-key dotemacs-leader-map "si" '("Jump to symbol" . imenu))
(define-key dotemacs-leader-map "sl" '("Jump to visible link" . link-hint-open-link))
(define-key dotemacs-leader-map "sm" '("Jump to bookmark" . bookmark-jump))
(define-key dotemacs-leader-map "ss" '("Search symbol at point" . dotemacs-search-symbol-at-point))

(define-key dotemacs-leader-map "t"  '("Toggle" . (keymap)))
(define-key dotemacs-leader-map "tf" '("Flycheck" . flycheck-mode))
(define-key dotemacs-leader-map "tl" '("Line numbers" . display-line-numbers-mode))
(define-key dotemacs-leader-map "ts" '("Spell checker" . flyspell-mode))
(define-key dotemacs-leader-map "tv" '("Visible mode" . visible-mode))
(define-key dotemacs-leader-map "tw" '("Soft line wrapping" . visual-line-mode))
(define-key dotemacs-leader-map "tz" '("Zen mode" . toggle-frame-fullscreen))

(global-set-key (kbd "C-r") 'consult-line)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

(provide 'dotemacs-keybinds)
;;; dotemacs-keybinds.el ends here
