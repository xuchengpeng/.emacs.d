;;; ui/dashboard/autoload/banner.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dashboard/choose-random-text-banner ()
  "Return the full path of a banner chosen randomly."
  (let* ((files (directory-files +dashboard-banner-directory t ".*\.txt"))
         (count (length files))
         (choice (random count)))
    (nth choice files)))

;;;###autoload
(defun +dashboard/choose-banner ()
  "Chooese banner to insert."
  (when +dashboard-startup-banner
    (cond ((eq 'official +dashboard-startup-banner)
           (+dashboard/get-banner-path 1))
          ((eq 'random +dashboard-startup-banner)
           (+dashboard/choose-random-text-banner))
          ((integerp +dashboard-startup-banner)
           (+dashboard/get-banner-path +dashboard-startup-banner))
          (t
           (+dashboard/get-banner-path 1)))))

;;;###autoload
(defun +dashboard/get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat +dashboard-banner-directory (format "%d.txt" index)))

;;;###autoload
(defun +dashboard/insert-ascii-banner-centered (file)
  "Insert the ascii banner contain in file and center it in the window.
FILE: the path to the file containing the banner."
  (insert
   (with-temp-buffer
     (insert-file-contents file)
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- +dashboard-buffer-window-width
                                         banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\s))
           (forward-line 1))))
     (buffer-string))))
