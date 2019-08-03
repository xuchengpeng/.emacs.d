;;; ui/themes/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +themes/set-color-theme (theme)
  "Switch to specific color THEME."
  (interactive
   (list
    (intern (completing-read "Switch to color theme: "
                             '(dotemacs-one
                               dotemacs-one-light
                               dotemacs-challenger-deep
                               dotemacs-city-lights
                               dotemacs-dracula
                               dotemacs-fairy-floss
                               dotemacs-gruvbox
                               dotemacs-Iosvkem
                               dotemacs-molokai
                               dotemacs-nord-light
                               dotemacs-nord
                               dotemacs-nova
                               dotemacs-opera-light
                               dotemacs-opera
                               dotemacs-peacock
                               dotemacs-solarized-dark
                               dotemacs-solarized-light
                               dotemacs-sourcerer
                               dotemacs-spacegrey
                               dotemacs-tomorrow-day
                               dotemacs-tomorrow-night
                               dotemacs-vibrant)))))
  (setq +themes|color-theme theme)
  (load-theme +themes|color-theme t))
