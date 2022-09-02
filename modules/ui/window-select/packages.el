;;; ui/window-select/packages.el -*- lexical-binding: t; -*-

(cond ((modulep! +switch-window)
       ;; Install switch-window if the user indicated the '+switch-window' module flag
       (package! switch-window))
      ((or (modulep! +ace-window) t)
       ;; Install ace-window if the user selects the flag '+ace-window' or by default
       ;; ... if the user did not specify a module flag
       (package! ace-window)))

(package! winum)
