;;; ui/themes/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +themes/set-color-theme (theme)
  "Switch to specific color THEME."
  (interactive
   (list
    (intern (completing-read "Switch to color theme: "
                             '(dotemacs-one
                               dotemacs-one-light
                               dotemacs-dracula
                               dotemacs-gruvbox
                               dotemacs-molokai
                               dotemacs-nord-light
                               dotemacs-nord
                               dotemacs-solarized-dark
                               dotemacs-solarized-light
                               dotemacs-tomorrow-day
                               dotemacs-tomorrow-night)))))
  (setq +themes-color-theme theme)
  (load-theme +themes-color-theme t))
