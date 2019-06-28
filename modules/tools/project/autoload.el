;;; tools/project/autoload.el -*- lexical-binding: t; -*-

(defvar projectile-project-root nil)

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This is necessary if
you want to interactive with a project other than the one you're in."
  `(let ((projectile-project-root-cache (make-hash-table :test 'equal))
         projectile-project-name
         projectile-project-root
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
(defun dotemacs-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (dotemacs-project-root dir)
       t))

;;;###autoload
(defun dotemacs-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun dotemacs-project-name (&optional dir)
  "Return the name of the current project.

Returns '-' if not in a valid project."
  (if-let* ((project-root (or (dotemacs-project-root dir)
                              (if dir (expand-file-name dir)))))
      (funcall projectile-project-name-function project-root)
    "-"))

;;;###autoload
(defun dotemacs-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (projectile-project-root dir)))

;;;###autoload
(defun dotemacs-project-find-file (dir)
 "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let* ((default-directory (file-truename (expand-file-name dir)))
         (project-root (dotemacs-project-root default-directory))
         (projectile-project-root default-directory)
         (projectile-enable-caching projectile-enable-caching))
    (cond ((and project-root (file-equal-p project-root projectile-project-root))
           (unless (dotemacs-project-p projectile-project-root)
             ;; Disable caching if this is not a real project; caching
             ;; non-projects easily has the potential to inflate the projectile
             ;; cache beyond reason.
             (setq projectile-enable-caching nil))
           (call-interactively
            ;; Intentionally avoid `helm-projectile-find-file', because it runs
            ;; asynchronously, and thus doesn't see the lexical `default-directory'
            (if (featurep! :completion ivy)
                #'counsel-projectile-find-file
              #'projectile-find-file)))
          ((fboundp 'project-find-file-in) ; emacs 26.1+ only
           (project-find-file-in nil (list default-directory) nil))
          ((fboundp 'counsel-file-jump) ; ivy only
           (call-interactively #'counsel-file-jump))
          ((fboundp 'helm-find-files)
           (call-interactively #'helm-find-files))
          ((call-interactively #'find-file)))))

;;;###autoload
(defun dotemacs-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively
     (cond ((featurep! :completion ivy)
            #'counsel-find-file)
           ((featurep! :completion helm)
            #'helm-find-files)
           (#'find-file)))))

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

