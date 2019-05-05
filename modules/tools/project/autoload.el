;;; tools/project/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This is necessary if
you want to interactive with a project other than the one you're in."
  `(let ((projectile-project-root-cache (make-hash-table :test 'equal))
         projectile-project-name
         projectile-require-project-root)
     ,@body))

;;;###autoload
(defmacro project-file-exists-p! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  `(file-exists-p! ,files (dotemacs-project-root)))

;;;###autoload
(defalias 'dotemacs-project-p #'projectile-project-p)

;;;###autoload
(defalias 'dotemacs-project-root #'projectile-project-root)

;;;###autoload
(defun dotemacs-project-name (&optional dir)
  "Return the name of the current project."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

;;;###autoload
(defun dotemacs-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (projectile-project-root dir)))

;;;###autoload
(defun dotemacs-project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (without-project-cache!
   (let* ((default-directory (file-truename dir))
          projectile-project-root)
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun dotemacs-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename dir)))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))

;;;###autoload
(defun dotemacs-project-buffer-list ()
  "Return a list of buffers belonging to the current project."
  (let ((buffers (dotemacs-buffer-list)))
    (if-let* ((project-root (dotemacs-project-root)))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun +project/kill-all-project-buffers ()
  "Kill all project buffers."
  (interactive "P")
  (switch-to-buffer (dotemacs-fallback-buffer))
  (dotemacs/cleanup-session (dotemacs-project-buffer-list)))

;;;###autoload
(defun +project/kill-other-project-buffers ()
  "Kill all other buffers (besides the current one).."
  (interactive "P")
  (let ((buffers (dotemacs-project-buffer-list))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (dotemacs-kill-buffer-and-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s project buffers" (length buffers)))))

;;;###autoload
(defun +project/kill-matching-project-buffers (pattern)
  "Kill buffers that match PATTERN in current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (dotemacs-project-buffer-list))
         (n (dotemacs-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s project buffers" n))))

