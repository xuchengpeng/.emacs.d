;;; completion/ivy/autoload/ivy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ivy/projectile-find-file ()
  "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME, `counsel-file-jump' if invoked from a
non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.

The point of this is to avoid Emacs locking up indexing massive file trees."
  (interactive)
  (call-interactively
   (cond ((or (file-equal-p default-directory "~")
              (when-let (proot (dotemacs-project-root))
                (file-equal-p proot "~")))
          #'counsel-find-file)

         ((dotemacs-project-p)
          (let ((files (projectile-current-project-files)))
            (if (<= (length files) ivy-sort-max-size)
                #'counsel-projectile-find-file
              #'projectile-find-file)))

         (#'counsel-file-jump))))

;;;###autoload
(cl-defun +ivy-file-search (&key query in all-files (recursive t))
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'counsel)
  (let* ((ivy-more-chars-alist '((t . 1)))
         (project-root (or (dotemacs-project-root) default-directory))
         (directory (or in project-root))
         (default-directory directory)
         (args (concat (if all-files " -uu")
                       (unless recursive " --maxdepth 1"))))
    (counsel-rg
     (or (if query query)
         (when (use-region-p)
           (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                 (end (or (bound-and-true-p evil-visual-end) (region-end))))
             (when (> (abs (- end beg)) 1)
               (let ((query (buffer-substring-no-properties beg end)))
                 ;; Escape characters that are special to ivy searches
                 (replace-regexp-in-string "[! |]" (lambda (substr)
                                                     (cond ((and (string= substr " ")
                                                                 (not (featurep! +fuzzy)))
                                                            "  ")
                                                           ((string= substr "|")
                                                            "\\\\\\\\|")
                                                           ((concat "\\\\" substr))))
                                           (rxt-quote-pcre query)))))))
     directory args
     (format "rg%s %s"
             args
             (cond ((equal directory default-directory)
                    "./")
                   ((equal directory project-root)
                    (projectile-project-name))
                   ((file-relative-name directory project-root)))))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.

If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+ivy-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+ivy/project-search arg initial-query default-directory))


;;
;;; Wrappers around `counsel-compile'

;;;###autoload
(defun +ivy/compile ()
  "Execute a compile command from the current buffer's directory."
  (interactive)
  (counsel-compile default-directory))

;;;###autoload
(defun +ivy/project-compile ()
  "Execute a compile command from the current project's root."
  (interactive)
  (counsel-compile (projectile-project-root)))
