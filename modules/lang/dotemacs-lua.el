;;; dotemacs-lua.el --- lua-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(lua-mode company-lua))

(defvar dotemacs-lua-mode-map (make-keymap)
  "Keymap for Lua commands.")

(use-package lua-mode
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :init
  (add-to-list 'dotemacs-company-backend-alist '(lua-mode company-lua company-capf company-dabbrev-code company-yasnippet))
  (add-to-list 'dotemacs-major-mode-map-alist '(lua-mode . dotemacs-lua-mode-map))
  (add-hook 'lua-mode-hook #'lsp-deferred)
  :config
  (setq lua-indent-level 2)

  (define-key dotemacs-lua-mode-map "b" 'lua-send-buffer)
  (define-key dotemacs-lua-mode-map "l" 'lua-send-current-line)
  (define-key dotemacs-lua-mode-map "f" 'lua-send-defun)
  (define-key dotemacs-lua-mode-map "p" 'lua-send-proc)
  (define-key dotemacs-lua-mode-map "r" 'lua-send-region)
  (define-key dotemacs-lua-mode-map "z" 'lua-show-process-buffer))

(use-package company-lua
  :defer t)

(provide 'dotemacs-lua)
;;; dotemacs-lua.el ends here
