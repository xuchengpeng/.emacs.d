;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +switch-window)
       (use-package switch-window
         :defer t))
      ((or (featurep! +ace-window) t)
       (use-package ace-window
         :defer t)))

(use-package winum
  :hook (buffer-list-update . winum-mode)
  :config
  (when (featurep! :ui modeline)
    (setq winum-auto-setup-mode-line nil)))
