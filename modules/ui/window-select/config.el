;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +switch-window)
       (use-package switch-window
         :bind ("M-o" . switch-window)))
      ((or (featurep! +ace-window) t)
       (use-package ace-window
         :bind ("M-o" . ace-window))))
