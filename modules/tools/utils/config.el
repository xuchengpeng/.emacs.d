;;; tools/utils/config.el -*- lexical-binding: t; -*-

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
    (exec-path-from-shell-initialize)))
