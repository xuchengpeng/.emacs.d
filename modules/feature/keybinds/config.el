;;; feature/keybinds/config.el -*- lexical-binding: t; -*-

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :commands which-key-mode
  :config (which-key-mode +1))

(add-hook 'after-init-hook
          (lambda () (load! "+bindings")))
