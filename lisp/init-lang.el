;;; init-lang.el --- Programming languages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/lang" dotemacs-dir) load-path)

(with-eval-after-load 'cc-mode
  (setq-default c-basic-offset 4))

(when (dotemacs-treesit-available-p)
  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4)))

(defun dotemacs-format-buffer (command &optional args)
  "Format current buffer with COMMAND and ARGS."
  (let ((command
         (if args
             (format "%s %s" command args)
           command)))
    (unless buffer-file-name
      (error "Save buffer before formatting"))
    (save-buffer)
    (shell-command
     (format "%s %s" command buffer-file-name))))

(require 'init-markdown)
(require 'init-python)
(require 'init-lua)

(provide 'init-lang)
;;; init-lang.el ends here
