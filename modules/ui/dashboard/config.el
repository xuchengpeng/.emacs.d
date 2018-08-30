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

(defun +dashboard/insert-banner ()
  "Insert banner."
  (setq +dashboard-buffer-window-width (if +dashboard-startup-buffer-responsive
                                           (window-width)
                                         80))
  (let ((banner (+dashboard/choose-banner))
        (buffer-read-only nil))
    (when banner
      (+dashboard/insert-ascii-banner-centered banner))))

(add-hook 'dotemacs-post-init-hook #'+dashboard/insert-banner t)
