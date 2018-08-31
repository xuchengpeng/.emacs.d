;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(load! "+banner")

(defconst +dashboard-buffer-window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `+dashboard-startup-buffer-responsive'.")

(defvar +dashboard-startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(defun +dashboard/init-dashboard ()
  "Initialize dashboard."
  (setq +dashboard-buffer-window-width (if +dashboard-startup-buffer-responsive
                                           (window-width)
                                         80))
  (+dashboard/insert-banner))

(add-hook 'dotemacs-post-init-hook #'+dashboard/init-dashboard t)
