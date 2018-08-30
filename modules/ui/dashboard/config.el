;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(defvar +dashboard-startup-banner 'official
   "Specify the startup banner. Default value is `official', it displays
the official spacemacs logo. An integer value is the index of text
banner, `random' chooses a random text banner in `core/banners'
directory. A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed.")

(defvar +dashboard-banner-directory (concat (DIR!) "banners/"))

(defconst +dashboard-buffer-window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `+dashboard-startup-buffer-responsive'.")

(defvar +dashboard-startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(defun +dashboard/init-dashboard ()
  "Initialize dashboard."
  (+dashboard/insert-banner))

(add-hook 'dotemacs-post-init-hook #'+dashboard/init-dashboard t)
