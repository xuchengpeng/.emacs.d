;;; emacs/keybinds/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/browse-project ()
  (interactive) (dotemacs-project-browse (dotemacs-project-root)))

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search-from-cwd)
           ((modulep! :completion ivy)   #'counsel-rg)
           ((modulep! :completion helm)  #'helm-do-grep-ag)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+default/search-cwd 'other))

;;;###autoload
(defun +default/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, prompt for a known project to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (if-let (projects (projectile-relevant-known-projects))
                  (completing-read "Search project: " projects
                                   nil t nil nil (dotemacs-project-root))
                (user-error "There are no known projects"))
            default-directory)))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search)
           ((modulep! :completion ivy)   #'counsel-projectile-rg)
           ((modulep! :completion helm)  #'helm-projectile-ag)
           (#'projectile-grep)))))

;;;###autoload
(defun +default/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+default/search-project 'other))
