;;; -*- lexical-binding: t; -*-

;;;###autoload
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
