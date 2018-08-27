;;; feature/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)


;;
;; Library
;;

(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))


;; --- Predicates -------------------------

;;;###autoload
(defalias #'+workspace-p #'perspective-p
  "Return t if OBJ is a perspective hash table.")

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")


;; --- Getters ----------------------------

;;;###autoload
(defalias #'+workspace-current #'get-current-persp
  "Return the currently active workspace.")

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let* ((persp (persp-get-by-name name)))
    (cond ((+workspace-p persp) persp)
          ((not noerror)
           (error "No workspace called '%s' was found" name)))))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (+workspace-current)))

;;;###autoload
(defun +workspace-list ()
  "Return a list of workspace structs (satisifes `+workspace-p')."
  (cdr (cl-loop for persp being the hash-values of *persp-hash*
                collect persp)))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  (cdr persp-names-cache))

;;;###autoload
(defun +workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP.

The buffer list is ordered by recency (same as `buffer-list').

PERSP can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (+workspace-current))))
    (unless (+workspace-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (cl-loop for buf in (buffer-list)
             if (+workspace-contains-buffer-p buf persp)
             collect buf)))

;;;###autoload
(defun +workspace-orphaned-buffer-list ()
  "Return a list of buffers that aren't associated with any perspective."
  (cl-remove-if #'persp--buffer-in-persps (buffer-list)))


;; --- Actions ----------------------------

;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (when (+workspace-exists-p name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name +workspaces-data-file persp-save-dir)
   *persp-hash* (list name))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-load-session (&optional name)
  "Replace current session with the entire session named NAME. If NAME is nil,
use `persp-auto-save-fname'."
  (mapc #'+workspace-delete (+workspace-list-names))
  (persp-load-state-from-file
   (expand-file-name (or name persp-auto-save-fname) persp-save-dir)))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name +workspaces-data-file persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))

;;;###autoload
(defun +workspace-save-session (&optional name)
  "Save a whole session as NAME. If NAME is nil, use `persp-auto-save-fname'.
Return t on success, nil otherwise."
  (let ((fname (expand-file-name (or name persp-auto-save-fname)
                                 persp-save-dir)))
    ;; disable auto-saving on kill-emacs if autosaving (i.e. name is nil)
    (when (or (not name)
              (string= name persp-auto-save-fname))
      (setq persp-auto-save-opt 0))
    (and (persp-save-state-to-file fname) t)))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (persp-add-new name))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-delete (name &optional inhibit-kill-p)
  "Delete the workspace denoted by NAME, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (when (+workspace--protected-p name)
    (error "Can't delete '%s' workspace" name))
  (+workspace-get name) ; error checking
  (persp-kill name inhibit-kill-p)
  (not (+workspace-exists-p name)))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (persp-frame-switch name)
  (equal (+workspace-current-name) name))


;;
;; Interactive commands
;;

;;;###autoload
(defun +workspace/load (name)
  "Load a workspace and switch to it."
  (interactive
   (list
    (completing-read
     "Workspace to load: "
     (persp-list-persp-names-in-file
      (expand-file-name +workspaces-data-file persp-save-dir)))))
  (if (not (+workspace-load name))
      (+workspace-error (format "Couldn't load workspace %s" name))
    (+workspace/switch-to name)
    (+workspace/display)))

;;;###autoload
(defun +workspace/save (name)
  "Save the current workspace."
  (interactive
   (list
    (completing-read "Workspace to save: " (+workspace-list-names))))
  (if (+workspace-save name)
      (+workspace-message (format "'%s' workspace saved" name) 'success)
    (+workspace-error (format "Couldn't save workspace %s" name))))

;;;###autoload
(defun +workspace/load-session (&optional name)
  "Load a session and switch to it."
  (interactive
   (list
    (completing-read
     "Session to load: "
     (directory-files persp-save-dir nil "^[^_.]")
     nil t)))
  (condition-case ex
      (let ((name (or name persp-auto-save-fname)))
        (+workspace-load-session name)
        (+workspace-message (format "'%s' workspace loaded" name) 'success))
    '(error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/load-last-session ()
  "Restore last session and switch to it."
  (interactive)
  (+workspace/load-session))

;;;###autoload
(defun +workspace/save-session (&optional name)
  "Save the current session."
  (interactive
   (list
    (completing-read
     "Save session as: "
     (directory-files persp-save-dir nil "^[^_.]"))))
  (condition-case-unless-debug ex
      (let ((name (or name persp-auto-save-fname)))
        (if (+workspace-save-session name)
            (+workspace-message (format "Saved session as '%s'" name) 'success)
          (error "Couldn't save session as '%s'" name)))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/rename (new-name)
  "Rename the current workspace."
  (interactive (list (read-from-minibuffer "New workspace name: ")))
  (condition-case-unless-debug ex
      (let* ((current-name (+workspace-current-name))
             (old-name (+workspace-rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (+workspace-message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/delete (name)
  "Delete this workspace."
  (interactive
   (let ((current-name (+workspace-current-name)))
     (list
      (completing-read (format "Delete workspace (default: %s): " current-name)
                       (+workspace-list-names)
                       nil nil current-name))))
  (let ((workspaces (+workspace-list-names)))
    (if (not (member name workspaces))
        (+workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
      (+workspace-delete name)
      (dotemacs/kill-all-buffers)
      (+workspace-message (format "Deleted '%s' workspace" name) 'success))))

;;;###autoload
(defun +workspace/kill-session ()
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive)
  (unless (cl-every #'+workspace-delete (+workspace-list-names))
    (+workspace-error "Could not clear session"))
  (dotemacs/kill-all-buffers))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Kill emacs without saving anything."
  (interactive)
  (let ((persp-auto-save-opt 0))
    (kill-emacs)))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive "iP")
  (unless name
    (setq name (format "#%s" (+workspace--generate-id))))
  (condition-case e
      (cond ((+workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (+workspace-switch name t)
             (persp-delete-other-windows)
             (switch-to-buffer (dotemacs-fallback-buffer))
             (+workspace/display)))
    ((debug error) (+workspace-error (cadr e) t))))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (completing-read "Switch to workspace: " (+workspace-list-names))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (unless (member index names)
                 (error "No workspace named %s" index))
               (+workspace-switch index))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/switch-to-last ()
  "Switch to the last workspace."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list-names)))))

;;;###autoload
(defun +workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, either
close the workspace (as well as its associated frame, if one exists) and move to
the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (dotemacs-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (let ((frame-persp (frame-parameter nil 'workspace)))
                 (if (string= frame-persp (+workspace-current-name))
                     (delete-frame)
                   (+workspace/delete current-persp-name))))

              (t (+workspace-error "Can't delete last workspace" t)))))))

;;
;; Tabs display in minibuffer
;;

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defun +workspace--message-body (message &optional type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

;;;###autoload
(defun +workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (minibuffer-message "%s" (+workspace--tabline))))


;;
;; Hooks
;;

;;;###autoload
(defun +workspaces|cleanup-unassociated-buffers ()
  "Kill leftover buffers that are unassociated with any perspective."
  (when persp-mode
    (cl-loop for buf in (buffer-list)
             unless (or (persp--buffer-in-persps buf)
                        (get-buffer-window buf))
             if (kill-buffer buf)
             sum 1)))

;;
;; Advice
;;

;;;###autoload
(defun +workspaces*autosave-real-buffers (orig-fn &rest args)
  "Don't autosave if no real buffers are open."
  (when (dotemacs-real-buffer-list)
    (apply orig-fn args))
  t)
