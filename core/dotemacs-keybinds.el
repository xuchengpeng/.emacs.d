;;; dotemacs-keybinds.el --- dotemacs keybindings.

;;; Commentary:
;;
;; dotemacs keybinds.
;;

;;; Code:

(defvar dotemacs-leader-map (make-sparse-keymap)
  "Leader map.")
(global-set-key (kbd "M-SPC") dotemacs-leader-map)

(defvar dotemacs-localleader-map (make-sparse-keymap)
  "Local leader map.")
(global-set-key (kbd "M-SPC m") dotemacs-localleader-map)

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
(define-key dotemacs-leader-map "ff" '("Find file" . find-file))
(define-key dotemacs-leader-map "fr" '("Recent files" . recentf-open-files))
(define-key dotemacs-leader-map "fs" '("Save file" . save-buffer))
(define-key dotemacs-leader-map "fw" '("Write file" . write-file))

(define-key dotemacs-leader-map "p"  '("Project" . (keymap)))
(define-key dotemacs-leader-map "pf" '("Find file in project" . projectile-find-file))
(define-key dotemacs-leader-map "po" '("Find other project file" . projectile-find-other-file))
(define-key dotemacs-leader-map "pr" '("Recent project files" . projectile-recentf))
(define-key dotemacs-leader-map "ps" '("Search project" . consult-ripgrep))
(define-key dotemacs-leader-map "pp" '("Switch project" . projectile-switch-project))

(define-key dotemacs-leader-map "s"  '("Search" . (keymap)))
(define-key dotemacs-leader-map "sb" '("Search buffer" . consult-line))
(define-key dotemacs-leader-map "sd" '("Search current directory" . dotemacs-search-cwd))
(define-key dotemacs-leader-map "sD" '("Search other directory" . dotemacs-search-other-cwd))
(define-key dotemacs-leader-map "si" '("Jump to symbol" . imenu))
(define-key dotemacs-leader-map "sl" '("Jump to visible link" . link-hint-open-link))
(define-key dotemacs-leader-map "sm" '("Jump to bookmark" . bookmark-jump))

(define-key dotemacs-leader-map "q"  '("Quit" . (keymap)))
(define-key dotemacs-leader-map "qq" '("Quit Emacs" . kill-emacs))
(define-key dotemacs-leader-map "qQ" '("Save and quit Emacs" . save-buffers-kill-terminal))

(provide 'dotemacs-keybinds)
;;; dotemacs-keybinds.el ends here
