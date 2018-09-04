;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(defconst +dashboard-buffer-name "*dashboard*"
	  "Dashboard's buffer name.")

(defconst +dashboard-buffer-window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `+dashboard-startup-buffer-responsive'.")

(defvar +dashboard-startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(load! "+banner")

(defvar +dashboard-widget-functions
  '(+dashboard/insert-banner
    +dashboard/insert-benchmark)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

;; helpers
(defun +dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +dashboard/insert-benchmark ()
  (insert
   "\n\n"
   (propertize
    (+dashboard--center
     +dashboard-buffer-window-width
     (dotemacs|display-benchmark 'return))
    'face 'font-lock-comment-face)
   "\n"))

(defun +dashboard/init-dashboard (&optional force)
  "Initialize dashboard."
  (let ((buffer-exists (buffer-live-p (get-buffer +dashboard-buffer-name)))
        (save-line nil))
    (when (or (not (eq +dashboard-buffer-window-width (window-width)))
              (not buffer-exists)
              force)
      (setq +dashboard-buffer-window-width (if +dashboard-startup-buffer-responsive
                                               (window-width)
                                             80))
      (with-current-buffer (get-buffer-create +dashboard-buffer-name)
        (let ((buffer-read-only nil)
              (list-separator "\n\n"))
          (erase-buffer)
          (run-hooks '+dashboard-widget-functions)))
      (switch-to-buffer +dashboard-buffer-name)
      (goto-char (point-min)))))

(defun +dashboard/resize-on-hook (&optional _)
  "Hook run on window resize events to redisplay the home buffer."
  (let ((space-win (get-buffer-window +dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (+dashboard/init-dashboard)))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-size-change-functions '+dashboard/resize-on-hook)
            (+dashboard/resize-on-hook)))

(add-hook 'dotemacs-post-init-hook #'+dashboard/init-dashboard t)
