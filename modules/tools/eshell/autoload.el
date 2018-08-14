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
