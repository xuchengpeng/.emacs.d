;;; ui/dashboard/+banner.el -*- lexical-binding: t; -*-


(defconst +dashboard-banner-directory (concat (dir!) "banners/"))

(defconst +dashboard-banner-official-png (concat +dashboard-banner-directory "emacs.png")
  "Emacs banner image.")

(defvar +dashboard-banner-logo-title "Welcome to Emacs!"
  "Specify the startup banner.")

(defvar +dashboard-startup-banner 'official
  "Specify the startup banner. Default value is `official', it displays
the official logo. An integer value is the index of text banner,
`random' chooses a random text banner in `banners' directory. A string
value must be a path to a .PNG file. If the value is nil then no banner
is displayed.

For example:
 'official
 'random
 1, 2 or ...
 \"emacs.png\"
 \"/home/.../*.png\"
 nil.")

(defun +dashboard|choose-random-text-banner ()
  "Return the full path of a banner chosen randomly."
  (let* ((files (directory-files +dashboard-banner-directory t ".*\.txt"))
         (count (length files))
         (choice (random count)))
    (nth choice files)))

(defun +dashboard|get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat +dashboard-banner-directory (format "%d.txt" index)))

(defun +dashboard|choose-banner ()
  "Chooese banner to insert."
  (when +dashboard-startup-banner
    (cond ((eq 'official +dashboard-startup-banner)
           (if (and (display-graphic-p) (image-type-available-p 'png))
              +dashboard-banner-official-png
            (+dashboard|get-banner-path 1)))
          ((eq 'random +dashboard-startup-banner)
           (+dashboard|choose-random-text-banner))
          ((integerp +dashboard-startup-banner)
           (+dashboard|get-banner-path +dashboard-startup-banner))
          ((and +dashboard-startup-banner
                (image-type-available-p (intern (file-name-extension
                                                 +dashboard-startup-banner)))
                (display-graphic-p))
           (if (file-exists-p! +dashboard-startup-banner +dashboard-banner-directory)
               (expand-file-name +dashboard-startup-banner +dashboard-banner-directory)
             (message (format "could not find banner %s"
                              +dashboard-startup-banner))
             (+dashboard|get-banner-path 1)))
          (t
           (+dashboard|get-banner-path 1)))))

(defun +dashboard|insert-ascii-banner-centered (file)
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

(defun +dashboard|insert-image-banner (banner)
  "Display an image banner.
BANNER: the path to an ascii banner file."
  (when (file-exists-p banner)
    (let* ((title +dashboard-banner-logo-title)
           (spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- +dashboard-buffer-window-width width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\s))
      (insert-image spec)
      (insert "\n\n")
      (when title
        (insert (make-string (max 0 (floor (/ (- +dashboard-buffer-window-width
                                                 (+ (length title) 1)) 2))) ?\s))
        (insert (format "%s\n\n" title))))))

(defun +dashboard|insert-banner ()
  "Insert banner."
  (let ((banner (+dashboard|choose-banner))
        (buffer-read-only nil))
    (when banner
      (if (image-type-available-p (intern (file-name-extension banner)))
          (+dashboard|insert-image-banner banner)
        (+dashboard|insert-ascii-banner-centered banner)))))
