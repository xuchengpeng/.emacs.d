;;; feature/keybinds/config.el -*- lexical-binding: t; -*-

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)
  (which-key-mode +1))

(add-hook 'after-init-hook
          (lambda ()
            (if (featurep! :feature evil)
                (load! "+evil-bindings")
              (load! "+emacs-bindings"))))
