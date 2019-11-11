;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(defconst +dashboard-buffer-name "*dashboard*"
	  "Dashboard's buffer name.")

(defconst +dashboard-buffer-window-width 80
  "Current width of the home buffer.")

(load! "+banner")

(defvar +dashboard-widget-functions
  '(+dashboard|insert-banner
    +dashboard|insert-benchmark)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

;;
;; Major mode

(define-derived-mode +dashboard-mode special-mode "Dashboard"
  :syntax-table nil
  :abbrev-table nil
  "Major mode for the dashboard buffer."
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2))

;; helpers
(defun +dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +dashboard|insert-benchmark ()
  (insert
   "\n\n"
   (propertize
    (+dashboard--center
     +dashboard-buffer-window-width
     (dotemacs-display-benchmark-h 'return))
    'face 'font-lock-comment-face)
   "\n"))

(defun +dashboard|init ()
  "Initialize dashboard."
  (let ((buffer-exists (buffer-live-p (get-buffer +dashboard-buffer-name)))
        (save-line nil))
    (when (or (not (eq +dashboard-buffer-window-width (window-width)))
              (not buffer-exists))
      (setq +dashboard-buffer-window-width (window-width))
      (with-current-buffer (get-buffer-create +dashboard-buffer-name)
        (let ((buffer-read-only nil)
              (list-separator "\n\n"))
          (erase-buffer)
          (run-hooks '+dashboard-widget-functions))
        (+dashboard-mode))
        (goto-char (point-min)))))

(defun +dashboard|resize-on-hook (&optional _)
  "Hook run on window resize events to redisplay the home buffer."
  (let ((space-win (get-buffer-window +dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (+dashboard|init)))))

(setq dotemacs-fallback-buffer-name +dashboard-buffer-name)

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-size-change-functions '+dashboard|resize-on-hook)
            (+dashboard|resize-on-hook)))

(defun +dashboard|startup-hook ()
  "Setup post initialization hooks.
If a command line argument is provided,
assume a filename and skip displaying Dashboard."
  (if (< (length command-line-args) 2 )
      (progn
        (add-hook 'after-init-hook (lambda ()
                                     ;; Display useful lists of items
                                     (+dashboard|init)))
        (add-hook 'emacs-startup-hook '(lambda ()
                                         (switch-to-buffer +dashboard-buffer-name)
                                         (goto-char (point-min))
                                         (redisplay))))))

(+dashboard|startup-hook)
