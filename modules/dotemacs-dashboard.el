;;; dotemacs-dashboard.el --- dashboard. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst dotemacs-dashboard--buffer-name "*dashboard*"
  "Dashboard's buffer name.")

(defconst dotemacs-dashboard--width 80
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
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (setq-local display-line-numbers-type nil)
  (setq-local hl-line-mode t))

(defun dotemacs-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun dotemacs-dashboard--widget-banner ()
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
  (define-key dotemacs-dashboard-mode-map "f" 'find-file)
  (define-key dotemacs-dashboard-mode-map "r" 'recentf-open-files)
  (define-key dotemacs-dashboard-mode-map "g" 'dotemacs-search-cwd)
  (define-key dotemacs-dashboard-mode-map "p" 'projectile-switch-project)
  (define-key dotemacs-dashboard-mode-map "b" 'bookmark-jump)
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
  (insert
    "\n\n"
    (propertize
      (dotemacs-dashboard--center dotemacs-dashboard--width (dotemacs-display-init-time 'return))
      'face 'font-lock-comment-face)
    "\n"))

(defun dotemacs-dashboard--reload ()
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
        (dotemacs-dashboard-mode)))))

(defun dotemacs-dashboard--resize (&optional _)
  (let ((space-win (get-buffer-window dotemacs-dashboard--buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (dotemacs-dashboard--reload)))))

(defun dotemacs-dashboard--init ()
  "Initializes dashboard."
  (add-to-list 'dotemacs-modeline-mode-alist '(dotemacs-dashboard-mode . dashboard))
  (when (equal (buffer-name) "*scratch*")
    (dotemacs-dashboard--reload))
  (add-hook 'window-size-change-functions #'dotemacs-dashboard--resize))

(add-hook 'window-setup-hook #'dotemacs-dashboard--init)

(provide 'dotemacs-dashboard)
;;; dotemacs-dashboard.el ends here
