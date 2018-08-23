;;; ui/themes/cofig.el  -*- lexical-binding: t; -*-

(use-package dotemacs-themes
  :load-path "modules/ui/themes/dotemacs-themes"
  :init
  (unless dotemacs-theme
    (setq dotemacs-theme 'dotemacs-one))
  :config
  ;; Enable flashing mode-line on errors
  (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-org-config)
  
  ;; For treemacs users
  (when (featurep! :ui treemacs)
    (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-treemacs-config))
  ;; or for neotree users
  (when (featurep! :ui neotree)
    (add-hook 'dotemacs-load-theme-hook #'dotemacs-themes-neotree-config)))

(defun dotemacs-set-theme (theme)
  "Set color theme."
  (cond
   ((eq theme 'default)
    (setq dotemacs-theme 'dotemacs-one))
  
   ((eq theme 'dark)
    (setq dotemacs-theme 'sanityinc-tomorrow-night))
  
   ((eq theme 'light)
    (setq dotemacs-theme 'sanityinc-tomorrow-day))
   
   ((string-prefix-p "dotemacs" (symbol-name theme))
    (setq dotemacs-theme theme))
   
   (t
    (error "Unknown color theme: '%s'" theme)))
  
  (message "Set color theme '%s'" dotemacs-theme))

;; Color theme
(dotemacs-set-theme dotemacs-color-theme)
