;;; core/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dotemacs/sudo-find-file (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":" (file-remote-p file 'user) "@" (file-remote-p file 'host)  "|sudo:root@" (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun dotemacs/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (dotemacs/sudo-find-file (file-truename buffer-file-name)))
