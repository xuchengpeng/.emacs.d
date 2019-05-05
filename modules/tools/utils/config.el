;;; tools/utils/config.el -*- lexical-binding: t; -*-

(if EMACS26+
    (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  (use-package nlinum
    :hook ((prog-mode text-mode conf-mode) . nlinum-mode)
    :config
    (setq nlinum-format "%4d"))
  (use-package nlinum-relative
    :commands (nlinum-relative-mode
               nlinum-relative-toggle
               nlinum-relative-on
               nlinum-relative-off)
    :config
    (setq nlinum-relative-current-symbol ""
          nlinum-relative-redisplay-delay 0)
    (when (featurep! :editor evil)
      (nlinum-relative-setup-evil))))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
    (exec-path-from-shell-initialize)))
