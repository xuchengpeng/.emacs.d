;;; tools/eshell/autoload.el -*- lexical-binding: t; -*-

(defun +eshell--current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

;;;###autoload
(defun +eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (+eshell--current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " λ " 'face 'font-lock-constant-face)))


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
