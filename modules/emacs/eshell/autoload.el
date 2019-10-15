;;; emacs/eshell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defface +eshell-prompt-pwd '((t :inherit font-lock-constant-face))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-git-branch '((t :inherit font-lock-builtin-face))
  "TODO"
  :group 'eshell)


(defun +eshell--current-git-branch ()
  (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                              if (string-match-p "^\*" match)
                              collect match))))
    (if (not (eq branch nil))
        (format " [%s]" (substring branch 2))
      "")))

;;;###autoload
(defun +eshell-default-prompt-fn ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (propertize (abbreviate-file-name (eshell/pwd))
                      'face '+eshell-prompt-pwd)
          (propertize (+eshell--current-git-branch)
                      'face '+eshell-prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))


;;;###autoload
(defun +eshell/search-history ()
  "Search the eshell command history with helm, ivy or `eshell-list-history'."
  (interactive)
  (cond ((featurep! :completion ivy)
         (require 'em-hist)
         (let* ((ivy-completion-beg (eshell-bol))
                (ivy-completion-end (point-at-eol))
                (input (buffer-substring-no-properties
                        ivy-completion-beg
                        ivy-completion-end)))
           ;; Better than `counsel-esh-history' because that doesn't
           ;; pre-populate the initial input or selection.
           (ivy-read "Command: "
                     (delete-dups
                      (when (> (ring-size eshell-history-ring) 0)
                        (ring-elements eshell-history-ring)))
                     :initial-input input
                     :action #'ivy-completion-in-region-action)))
        ((featurep! :completion helm)
         (helm-eshell-history))
        ((eshell-list-history))))

;;;###autodef
(defun set-eshell-alias! (&rest aliases)
  "Define aliases for eshell.

ALIASES is a flat list of alias -> command pairs. e.g.

  (set-eshell-alias!
    \"hi\"  \"echo hello world\"
    \"bye\" \"echo goodbye world\")"
  (or (cl-evenp (length aliases))
      (signal 'wrong-number-of-arguments (list 'even (length aliases))))
  (after! eshell
    (while aliases
      (let ((alias (pop aliases))
            (command (pop aliases)))
        (if-let* ((oldval (assoc alias +eshell-aliases)))
            (setcdr oldval (list command))
          (push (list alias command) +eshell-aliases))))
    (when (boundp 'eshell-command-aliases-list)
      (if +eshell--default-aliases
          (setq eshell-command-aliases-list
                (append +eshell--default-aliases
                        +eshell-aliases))
        (setq eshell-command-aliases-list +eshell-aliases)))))

