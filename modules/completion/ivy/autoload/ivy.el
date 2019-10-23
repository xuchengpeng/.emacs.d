;;; completion/ivy/autoload/ivy.el -*- lexical-binding: t; -*-

(defvar +ivy-project-search-engines '(rg ag)
  "What search tools for `+ivy/project-search' (and `+ivy-file-search' when no
ENGINE is specified) to try, and in what order.

To disable a particular tool, remove it from this list. To prioritize a tool
over others, move it to the front of the list. Later duplicates in this list are
silently ignored.

If you want to already use git-grep or grep, set this to nil.")

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

(defvar +ivy-file-search-shell
  (or (executable-find "dash")
      (executable-find "sh")
      shell-file-name)
  "The SHELL to invoke ag/rg/pt/git-grep/grep searchs from.

This only affects `+ivy/*' search commands (e.g. `+ivy/rg' and
`+ivy/project-search').

By default, this the most basic, uncustomized shell, to prevent interference
caused by slow shell configs at the cost of isolating these programs from
envvars that may have been set in the user's shell config to change their
behavior. If this bothers you, change this to `shell-file-name'.")

;;;###autoload
(cl-defun +ivy-file-search (engine &key query in all-files (recursive t))
  "Conduct a file search using ENGINE, which can be any of: rg, ag, pt, and
grep. If omitted, ENGINE will default to the first one it detects, in that
order.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (let* ((project-root (or (dotemacs-project-root) default-directory))
         (directory (or in project-root))
         (default-directory directory)
         (engine (or engine
                     (cl-loop for tool in +ivy-project-search-engines
                              if (executable-find (symbol-name tool))
                              return tool)
                     (and (or (executable-find "grep")
                              (executable-find "git"))
                          'grep)
                     (error "No search engine specified (is ag, rg, pt or git installed?)")))
         (query
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
                                                                ((and (string= substr "|")
                                                                      (eq engine 'rg))
                                                                 "\\\\\\\\|")
                                                                ((concat "\\\\" substr))))
                                                (rxt-quote-pcre query))))))))
         (prompt
          (format "%s%%s %s"
                  (symbol-name engine)
                  (cond ((equal directory default-directory)
                         "./")
                        ((equal directory project-root)
                         (projectile-project-name))
                        ((file-relative-name directory project-root))))))
    (require 'counsel)
    (let ((ivy-more-chars-alist
           (if query '((t . 1)) ivy-more-chars-alist))
          (shell-file-name +ivy-file-search-shell))
      (pcase engine
        (`grep
         (let ((counsel-projectile-grep-initial-input query))
           (cl-letf (((symbol-function #'counsel-locate-git-root)
                      (lambda () directory)))
             (if all-files
                 (cl-letf (((symbol-function #'projectile-ignored-directories-rel)
                            (symbol-function #'ignore))
                           ((symbol-function #'projectile-ignored-files-rel)
                            (symbol-function #'ignore)))
                   (counsel-projectile-grep))
               (counsel-projectile-grep)))))
        (`ag
         (let ((args (concat (if all-files " -a")
                             (unless recursive " --depth 1"))))
           (counsel-ag query directory args (format prompt args))))
        (`rg
         (let ((args (concat (if all-files " -uu")
                             (unless recursive " --maxdepth 1"))))
           (counsel-rg query directory args (format prompt args))))
        (_ (error "No search engine specified"))))))

(defun +ivy--get-command (format)
  (cl-loop for tool in (cl-remove-duplicates +ivy-project-search-engines :from-end t)
           if (executable-find (symbol-name tool))
           return (intern (format format tool))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a project search from the project root.

Uses the first available search backend from `+ivy-project-search-engines'. If
ARG (universal argument), include all files, even hidden or compressed ones, in
the search."
  (interactive "P")
  (funcall (or (+ivy--get-command "+ivy/%s")
               #'+ivy/grep)
           arg
           initial-query
           directory))

;;;###autoload
(defun +ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

Uses the first available search backend from `+ivy-project-search-engines'. If
ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (funcall (or (+ivy--get-command "+ivy/%s-from-cwd")
               #'+ivy/grep-from-cwd)
           arg
           initial-query))

;;;###autoload (autoload '+ivy/rg "completion/ivy/autoload/ivy" nil t)
;;;###autoload (autoload '+ivy/rg-from-cwd "completion/ivy/autoload/ivy" nil t)
;;;###autoload (autoload '+ivy/ag "completion/ivy/autoload/ivy" nil t)
;;;###autoload (autoload '+ivy/ag-from-cwd "completion/ivy/autoload/ivy" nil t)
;;;###autoload (autoload '+ivy/grep "completion/ivy/autoload/ivy" nil t)
;;;###autoload (autoload '+ivy/grep-from-cwd "completion/ivy/autoload/ivy" nil t)

(dolist (engine `(,@(cl-remove-duplicates +ivy-project-search-engines :from-end t) grep))
  (defalias (intern (format "+ivy/%s" engine))
    (lambda (all-files-p &optional query directory)
      (interactive "P")
      (+ivy-file-search engine :query query :in directory :all-files all-files-p))
    (format "Perform a project file search using %s.

QUERY is a regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, search compressed and hidden files as well."
            engine))

  (defalias (intern (format "+ivy/%s-from-cwd" engine))
    (lambda (all-files-p &optional query)
      (interactive "P")
      (+ivy-file-search engine :query query :in default-directory :all-files all-files-p))
    (format "Perform a project file search from the current directory using %s.

QUERY is a regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, search compressed and hidden files as well."
            engine)))

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
