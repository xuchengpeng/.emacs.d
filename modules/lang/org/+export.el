;;; lang/org/+export.el -*- lexical-binding: t; -*-

(use-package ox-pandoc
  :after ox
  :config
  (setq org-export-directory (concat org-directory ".export/")
        org-export-backends '(ascii html latex md))
  
  (when (executable-find "pandoc")
    (add-to-list 'org-export-backends 'pandoc nil #'eq))
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)))
  
  ;; Export to a central location by default or if target isn't in
  ;; `org-directory'.
  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `org-directory'."
    (when (and (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p buffer-file-name org-directory))
      (cl-destructuring-bind (extension &optional subtreep _pubdir) args
        (let ((dir org-export-directory))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (setq args (list extension subtreep dir)))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))

(use-package ox-hugo
  :after ox)

(after-load! 'org-capture
  (defun dotemacs-org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
     See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title))
           (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time))))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DATE: " date)
                   ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :comments true :mathjax false"
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))
  
  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Hugo Posts" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "hugo-posts.org" "Hugo Posts")
                 (function dotemacs-org-hugo-new-subtree-post-capture-template))))
