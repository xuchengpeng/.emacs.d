;;; feature/utils/config.el -*- lexical-binding: t; -*-

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :init
  (which-key-mode 1))

(use-package hydra
  :bind (("C-c f" . hydra-flycheck/body)
         ("C-c t" . hydra-toggle/body)
         ("C-c w" . hydra-window/body)))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
    (exec-path-from-shell-initialize)))
