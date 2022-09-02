;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(cond ((modulep! +switch-window)
       (use-package switch-window
         :defer t
         :init
         (define-key!
           [remap other-window] 'switch-window)))
      ((or (modulep! +ace-window) t)
       (use-package ace-window
         :defer t
         :init
         (define-key!
           [remap other-window] 'ace-window))))

(use-package winum
  :hook (after-init . winum-mode))
