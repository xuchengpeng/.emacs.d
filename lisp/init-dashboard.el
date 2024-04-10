;;; init-dashboard.el --- dashboard. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar dotemacs-dashboard--buffer-name "*dashboard*"
  "Dashboard's buffer name.")

(defvar dotemacs-dashboard--width 80
  "Current width of the home buffer.")

(defvar dotemacs-dashboard--functions
  '(dotemacs-dashboard--widget-banner
    dotemacs-dashboard--widget-menu
    dotemacs-dashboard--widget-loaded)
  "List of widget functions.")

(define-derived-mode dotemacs-dashboard-mode special-mode "Dashboard"
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
  (setq-local display-line-numbers-type nil)
  (setq-local global-hl-line-mode nil)
  (setq-local dotemacs-modeline-left '(dotemacs-modeline--window-number
                                       dotemacs-modeline--buffer-default-directory)
              dotemacs-modeline-right '(dotemacs-modeline--major-mode)))

(defun dotemacs-dashboard--center (len s)
  "Center S with LEN."
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun dotemacs-dashboard--widget-banner ()
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
        (insert (dotemacs-dashboard--center
                 dotemacs-dashboard--width
                 (concat
                  line (make-string (max 0 (- longest-line (length line)))
                                    32)))
                "\n"))
      'face 'font-lock-keyword-face)))

(defun dotemacs-dashboard--widget-menu ()
  "Widget menu."
  (keymap-set dotemacs-dashboard-mode-map "f" 'find-file)
  (keymap-set dotemacs-dashboard-mode-map "r" 'recentf-open-files)
  (keymap-set dotemacs-dashboard-mode-map "g" 'consult-ripgrep)
  (keymap-set dotemacs-dashboard-mode-map "p" 'project-switch-project)
  (keymap-set dotemacs-dashboard-mode-map "b" 'bookmark-jump)
  (let* ((menu
          '(("Find file" . "f")
            ("Recent files" . "r")
            ("Grep text" . "g")
            ("Open project" . "p")
            ("Jump to bookmark" . "b"))))
    (insert "\n\n")
    (dolist (line menu (point))
      (insert (dotemacs-dashboard--center
               dotemacs-dashboard--width
               (concat "> "
                       (car line)
                       (make-string (max 0 (- 40 (length (car line)))) 32)
                       (propertize (cdr line) 'face 'font-lock-type-face)))
              "\n\n"))))

(defun dotemacs-dashboard--widget-loaded ()
  "Show packages loaded time."
  (insert
   "\n\n"
   (propertize
    (dotemacs-dashboard--center
     dotemacs-dashboard--width
     (format "dotemacs loaded %d packages in %.2fms"
             (length package-activated-list)
             (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))))
    'face 'font-lock-comment-face)
   "\n"))

(defun dotemacs-dashboard--reload ()
  "Reload dashboard."
  (when (or (not (eq dotemacs-dashboard--width (window-width)))
            (not (buffer-live-p (get-buffer dotemacs-dashboard--buffer-name))))
    (setq dotemacs-dashboard--width (window-width))
    (with-current-buffer (get-buffer-create dotemacs-dashboard--buffer-name)
      (set-window-buffer nil (current-buffer))
      (let ((pt (point))
            (buffer-read-only nil))
        (erase-buffer)
        (run-hooks 'dotemacs-dashboard--functions)
        (goto-char pt))
      (unless (eq major-mode 'dotemacs-dashboard-mode)
        (dotemacs-dashboard-mode))
      (current-buffer))))

(defun dotemacs-dashboard--resize (&optional _)
  "Resize dashboard."
  (let ((space-win (get-buffer-window dotemacs-dashboard--buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (dotemacs-dashboard--reload)))))

(defun dotemacs-dashboard-init ()
  "Initialize dashboard."
  (when (equal (buffer-name) "*scratch*")
    (dotemacs-dashboard--reload))
  (add-hook 'window-configuration-change-hook #'dotemacs-dashboard--resize)
  (add-hook 'window-size-change-functions #'dotemacs-dashboard--resize))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
