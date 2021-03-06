;;; tools/project/autoload.el -*- lexical-binding: t; -*-

(defvar projectile-project-root nil)

;;;###autoload (autoload 'projectile-relevant-known-projects "projectile")

;;
;;; Macros

;;;###autoload
(defmacro project-file-exists-p! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  `(file-exists-p! ,files (dotemacs-project-root)))

;;
;;; Commands

;;;###autoload
(defun dotemacs/find-file-in-other-project (project-root)
  "Preforms `projectile-find-file' in a known project of your choosing."
  (interactive
   (list
    (completing-read "Find file in project: " (projectile-relevant-known-projects)
                     nil nil nil nil (dotemacs-project-root))))
  (unless (file-directory-p project-root)
    (error "Project directory '%s' doesn't exist" project-root))
  (dotemacs-project-find-file project-root))

;;;###autoload
(defun dotemacs/browse-in-other-project (project-root)
  "Preforms `find-file' in a known project of your choosing."
  (interactive
   (list
    (completing-read "Browse in project: " (projectile-relevant-known-projects)
                     nil nil nil nil (dotemacs-project-root))))
  (unless (file-directory-p project-root)
    (error "Project directory '%s' doesn't exist" project-root))
  (dotemacs-project-browse project-root))

;;
;;; Library

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
            ;; asynchronously, and thus doesn't see the lexical
            ;;  `default-directory'
            (if (featurep! :completion ivy)
                #'counsel-projectile-find-file
              #'projectile-find-file)))
          ((fboundp 'counsel-file-jump) ; ivy only
           (call-interactively #'counsel-file-jump))
          ((project-current)
           (project-find-file-in nil (list default-directory) nil))
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
