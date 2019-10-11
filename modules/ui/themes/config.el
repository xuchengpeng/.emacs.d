;;; ui/themes/config.el -*- lexical-binding: t; -*-

(defvar +themes|color-theme nil
  "The color theme to load.")

(defvar +themes-dir (concat (dir!) "/dotemacs-themes")
  "Directoriy for your private themes.")

(defun +themes|init-theme ()
  "Set the theme."
  (add-to-list 'load-path +themes-dir)
  
  (require 'dotemacs-themes)
  
  ;; Enable flashing mode-line on errors
  (add-hook 'dotemacs-load-theme-hook (lambda! (require 'dotemacs-themes-ext-visual-bell)
                                               (dotemacs-themes-visual-bell-config)))
  
  ;; Corrects (and improves) org-mode's native fontification.
  (add-hook 'dotemacs-load-theme-hook (lambda! (require 'dotemacs-themes-ext-org)))
  
  ;; For treemacs users
  (when (featurep! :ui treemacs)
    (add-hook 'dotemacs-load-theme-hook (lambda! (require 'dotemacs-themes-ext-treemacs))))
  ;; or for neotree users
  (when (featurep! :ui neotree)
    (add-hook 'dotemacs-load-theme-hook (lambda! (require 'dotemacs-themes-ext-neotree))))
  
  (unless +themes|color-theme
    (setq +themes|color-theme 'dotemacs-one))
  
  (when (and +themes|color-theme (not (memq +themes|color-theme custom-enabled-themes)))
    (load-theme +themes|color-theme t)))

(add-hook 'dotemacs-init-ui-hook #'+themes|init-theme)
