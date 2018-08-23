;;; ui/themes/cofig.el  -*- lexical-binding: t; -*-

(defun dotemacs-set-custom-theme (theme)
  "Set dotemacs-themes"
  (require 'dotemacs-themes)
  
  ;; Global settings (defaults)
  (setq dotemacs-themes-enable-bold t    ; if nil, bold is universally disabled
        dotemacs-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Load the theme (dotemacs-one, dotemacs-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme theme t)
  
  ;; Enable flashing mode-line on errors
  (dotemacs-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (dotemacs-themes-org-config)
  
  ;; For treemacs users
  (dotemacs-themes-treemacs-config)
  ;; or for neotree users
  ;;(dotemacs-themes-neotree-config)
  
  (message "Set custom color theme '%s'." theme))

(defun dotemacs-set-theme (theme)
  "Set color theme."
  (cond
   ((eq theme 'default)
    (dotemacs-set-custom-theme 'dotemacs-one))
  
   ((eq theme 'dark)
    (load-theme 'sanityinc-tomorrow-night t))
  
   ((eq theme 'light)
    (load-theme 'sanityinc-tomorrow-day t))
   
   ((string-prefix-p "dotemacs" (symbol-name theme))
     (dotemacs-set-custom-theme theme))
   
   (t
    (error "Unknown color theme: '%s'" theme))))

(let ((dir (concat (DIR!) "dotemacs-themes/")))
  (add-to-list 'load-path dir))

;; Color theme
(dotemacs-set-theme dotemacs-color-theme)
