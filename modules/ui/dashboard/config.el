;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(defconst +dashboard-buffer-name "*dashboard*"
	  "Dashboard's buffer name.")

(defconst +dashboard-buffer-window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `+dashboard-startup-buffer-responsive'.")

(defvar +dashboard-startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(load! "+banner")

(defvar +dashboard-buffer-last-width nil
  "Previous width of dashboard-buffer.")

(defun +dashboard/init-dashboard ()
  "Initialize dashboard."
  (let ((buffer-exists (buffer-live-p (get-buffer +dashboard-buffer-name)))
        (save-line nil))
    (when (or (not (eq +dashboard-buffer-last-width (window-width)))
              (not buffer-exists))
      (setq +dashboard-buffer-window-width (if +dashboard-startup-buffer-responsive
                                               (window-width)
                                             80)
            +dashboard-buffer-last-width +dashboard-buffer-window-width)
      (with-current-buffer (get-buffer-create +dashboard-buffer-name)
        (let ((buffer-read-only nil)
              (list-separator "\n\n"))
          (erase-buffer)
          (+dashboard/insert-banner)))
      (switch-to-buffer +dashboard-buffer-name)
      (goto-char (point-min)))))

(add-hook 'dotemacs-post-init-hook #'+dashboard/init-dashboard t)
