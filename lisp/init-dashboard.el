;;; init-dashboard.el --- dashboard  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar dashboard-name "*dashboard*"
  "Dashboard's buffer name.")

(defvar dashboard-widgets
  '(dashboard--banner
    dashboard--menu
    dashboard--startuptime)
  "List of widgets.")

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Major mode for the dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (setq-local display-line-numbers-type nil))

(defvar dashboard--width 40
  "Current width of the home buffer.")

(defun dashboard--center (len s)
  "Center S with LEN."
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun dashboard--banner ()
  "Widget banner."
  (let* ((banner
          '("███████╗███╗   ███╗ █████╗  ██████╗███████╗"
            "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
            "█████╗  ██╔████╔██║███████║██║     ███████╗"
            "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
            "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
            "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (insert "\n\n\n\n")
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (dashboard--center
                dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'font-lock-keyword-face)))

(defun dashboard--menu ()
  "Widget menu."
  (keymap-set dashboard-mode-map "f" 'find-file)
  (keymap-set dashboard-mode-map "r" 'recentf-open-files)
  (keymap-set dashboard-mode-map "g" 'consult-ripgrep)
  (keymap-set dashboard-mode-map "p" 'project-switch-project)
  (keymap-set dashboard-mode-map "a" 'org-agenda)
  (let* ((menu
          '(("Find file" . "f")
            ("Recently opened files" . "r")
            ("Grep text" . "g")
            ("Open project" . "p")
            ("Agenda" . "a"))))
    (insert "\n\n")
    (dolist (line menu (point))
      (insert (dashboard--center
               dashboard--width
               (concat "> "
                       (car line)
                       (make-string (max 0 (- 40 (length (car line)))) 32)
                       (propertize (cdr line) 'face 'font-lock-type-face)))
              "\n\n"))))

(defun dashboard--startuptime ()
  "Show packages loaded time."
  (insert
   "\n\n"
   (propertize
    (dashboard--center
     dashboard--width
     (format "Emacs loaded %d packages in %.3fs"
             (length package-activated-list)
             (float-time (time-subtract after-init-time before-init-time))))
    'face 'font-lock-comment-face)
   "\n"))

(defun dashboard--reload ()
  "Reload dashboard."
  (unless (buffer-live-p (get-buffer dashboard-name))
    (with-current-buffer (get-buffer-create "*scratch*")
      (rename-buffer dashboard-name)
      (set-window-buffer nil (current-buffer))
      (let ((pt (point)))
        (erase-buffer)
        (run-hooks 'dashboard-widgets)
        (goto-char pt))
      (unless (eq major-mode 'dashboard-mode)
        (dashboard-mode))
      (dashboard--resize)
      (current-buffer))))

(defun dashboard--resize (&optional _)
  "Resize dashboard."
  (ignore-errors
    (let (buffer-list-update-hook
          window-configuration-change-hook
          window-size-change-functions)
      (when-let* ((windows (get-buffer-window-list dashboard-name nil t)))
        (dolist (win windows)
          (set-window-start win 0)
          (set-window-fringes win 0)
          (set-window-margins win (max 0 (/ (- (window-total-width win) dashboard--width) 2))))))))

(defun dashboard-initialize ()
  "Initialize dashboard."
  (dashboard--reload)
  (add-hook 'window-configuration-change-hook #'dashboard--resize)
  (add-hook 'window-size-change-functions #'dashboard--resize))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
