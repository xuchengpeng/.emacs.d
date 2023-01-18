;;; core/autoload/files.el -*- lexical-binding: t; -*-

(defun dotemacs--sudo-file (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun dotemacs/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (dotemacs--sudo-file file)))

;;;###autoload
(defun dotemacs/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-alternate-file (dotemacs--sudo-file (or buffer-file-name
                                                (when (or (derived-mode-p 'dired-mode)
                                                          (derived-mode-p 'wdired-mode))
                                                  default-directory)))))
