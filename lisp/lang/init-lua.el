;;; init-lua.el --- lua-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'lua-mode)

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :config
  (setq lua-indent-level 2)

  (defvar-keymap dotemacs-lua-mode-map
    :doc "Keymap for lua-mode."
    "b" 'lua-send-buffer
    "l" 'lua-send-current-line
    "f" 'lua-send-defun
    "p" 'lua-send-proc
    "r" 'lua-send-region
    "z" 'lua-show-process-buffer)

  (add-hook 'lua-mode-hook
            (lambda ()
              (keymap-local-set dotemacs-localleader-key dotemacs-lua-mode-map)
              (when (and (dotemacs-treesit-available-p)
                         (treesit-language-available-p 'lua))
                (treesit-parser-create 'lua))))

  (reformatter-define lua-format
    :program "stylua"))

(provide 'init-lua)
;;; init-lua.el ends here
