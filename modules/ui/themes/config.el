;;; ui/themes/config.el -*- lexical-binding: t; -*-

(defvar +themes|color-theme nil
  "The color theme to load.")

(use-package dotemacs-themes
  :load-path "modules/ui/themes/dotemacs-themes"
  :defer t
  :init
  (unless +themes|color-theme
    (setq +themes|color-theme 'dotemacs-one))
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

(defun +themes|init-theme ()
  "Set the theme."
  (require 'dotemacs-themes)
  (when (and +themes|color-theme (not (memq +themes|color-theme custom-enabled-themes)))
    (load-theme +themes|color-theme t)))

(add-hook 'dotemacs-init-ui-hook #'+themes|init-theme)
