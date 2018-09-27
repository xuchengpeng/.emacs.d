;;; ui/modeline-old/config.el --- Initialize modeline. -*- lexical-binding: t; -*-

;;
;; Variables

(defvar dotemacs-modeline-height 22
  "How tall the mode-line should be (only respected in GUI Emacs).")

;; externs
(defvar text-scale-mode-amount)
(defvar flycheck-current-errors)

;;
;; Custom faces

(defgroup dotemacs-modeline nil
  ""
  :group 'dotemacs)

(defface dotemacs-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*flycheck'."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'."
  :group 'dotemacs-modeline)

;; Bar
(defface dotemacs-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group 'dotemacs-modeline)


(defmacro dotemacs-modeline-def-modeline-segment (name &rest forms)
  "Defines a modeline segment and byte compiles it with NAME and FORMS."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "dotemacs-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst dotemacs--prepare-modeline-segments (segments)
  "Prepare modelin SEGMENTS."
  (cl-loop for seg in segments
           if (stringp seg)
           collect seg
           else
           collect (list (intern (format "dotemacs-modeline-segment--%s" (symbol-name seg))))))

(defmacro dotemacs-modeline-def-modeline (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `dotemacs-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`dotemacs-modeline-def-modeline-segment'.
Example:
  (dotemacs-modeline-def-modeline minimal
    (bar \" \" buffer-info)
    (media-info major-mode))
  (dotemacs-set-modeline 'minimal t)"
  (let ((sym (intern (format "dotemacs-modeline-format--%s" name)))
        (lhs-forms (dotemacs--prepare-modeline-segments lhs))
        (rhs-forms (dotemacs--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun dotemacs-modeline (key)
  "Return a mode-line configuration associated with KEY (a symbol).
Throws an error if it doesn't exist."
  (let ((fn (intern (format "dotemacs-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun dotemacs-set-modeline (key &optional default)
  "Set the modeline format.  Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let* ((modeline (dotemacs-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))

;; Keep `dotemacs-modeline-current-window' up-to-date
(defvar dotemacs-modeline-current-window (frame-selected-window))
(defun dotemacs-modeline|set-selected-window (&rest _)
  "Set `dotemacs-modeline-current-window' appropriately."
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq dotemacs-modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'dotemacs-modeline|set-selected-window)
(add-hook 'focus-in-hook #'dotemacs-modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'dotemacs-modeline|set-selected-window)
(advice-add #'select-window :after #'dotemacs-modeline|set-selected-window)

;;
;; Modeline helpers

(defsubst dotemacs-modeline--active ()
  "Check if modelne is active."
  (eq (selected-window) dotemacs-modeline-current-window))

;; Inspired from `powerline's `pl/make-xpm'.
(defun dotemacs-modeline--make-xpm (color height width)
  "Create an XPM bitmap with COLOR, HEIGHT and WIDTH."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defvar dotemacs-modeline-buffer-name-function
  #'dotemacs-modeline--file-path
  "TODO.")

(defun dotemacs-modeline-project-root ()
  "Return the root of your project, or `default-directory' if none was found."
  (when (fboundp 'projectile-project-root)
    (let (projectile-require-project-root)
      (projectile-project-root))))

(defun dotemacs-modeline--file-path (&optional path)
  "PATH."
  (let ((buffer-file-name (or path buffer-file-name))
        (root (dotemacs-modeline-project-root))
        (active (dotemacs-modeline--active)))
    (cond ((null root)
           (propertize "%b" 'face (if active 'dotemacs-modeline-buffer-file)))
          ((or (null buffer-file-name)
               (directory-name-p buffer-file-name))
           (propertize (abbreviate-file-name (or buffer-file-name default-directory))
                       'face (if active 'dotemacs-modeline-buffer-path)))
          ((let* ((modified-faces (if (buffer-modified-p) 'dotemacs-modeline-buffer-modified))
                  (true-filename (file-truename buffer-file-name))
                  (relative-dirs (file-relative-name (file-name-directory true-filename)
                                                     (concat root "../")))
                  (relative-faces (or modified-faces (if active 'dotemacs-modeline-buffer-path)))
                  (file-faces (or modified-faces (if active 'dotemacs-modeline-buffer-file))))
             (if (equal "./" relative-dirs) (setq relative-dirs ""))
             (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                     (propertize (file-name-nondirectory true-filename)
                                 'face (if file-faces `(:inherit ,file-faces)))))))))

;;
;; Segments

(dotemacs-modeline-def-modeline-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

(dotemacs-modeline-def-modeline-segment buffer-info
  (concat " "
          (if buffer-file-name "%I ")
          (if buffer-file-name
              (funcall dotemacs-modeline-buffer-name-function buffer-file-name)
            "%b")))

;;
(dotemacs-modeline-def-modeline-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat " "
          (if buffer-file-name "%I ")
          (propertize
           "%b"
           'face (cond ((and buffer-file-name (buffer-modified-p))
                        'dotemacs-modeline-buffer-modified)
                       ((dotemacs-modeline--active) 'dotemacs-modeline-buffer-file)))))

;;
(dotemacs-modeline-def-modeline-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

;;
(dotemacs-modeline-def-modeline-segment major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-major-mode)))

;;
(dotemacs-modeline-def-modeline-segment vcs
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (dotemacs-modeline--active)))
        (cond ((memq state '(edited added))
                (if active (setq face 'dotemacs-modeline-info)))
               ((eq state 'needs-merge)
                (if active (setq face 'dotemacs-modeline-info)))
               ((eq state 'needs-update)
                (if active (setq face 'dotemacs-modeline-warning)))
               ((memq state '(removed conflict unregistered))
                (if active (setq face 'dotemacs-modeline-urgent)))
               (t
                (if active (setq face 'dotemacs-modeline-info))))
        (concat " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face)))))))

;;
(dotemacs-modeline-def-modeline-segment flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      (`finished
       (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
              (no-errors (cdr (assq 'error error-counts)))
              (no-warnings (cdr (assq 'warning error-counts)))
              (face (cond (no-errors 'dotemacs-modeline-urgent)
                          (no-warnings 'dotemacs-modeline-warning)
                          (t 'dotemacs-modeline-info))))
         (propertize (format " [%s/%s]" (or no-errors 0) (or no-warnings 0))
                     'face (if (dotemacs-modeline--active) face))))
      (`interrupted " -")
      (`suspicious '(propertize " ?" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-warning)))
      (`running (propertize " ?" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-info)))
      (`errored (propertize " !" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-urgent)))
      (`no-checker (propertize " -" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-warning)))
      ;; (`not-checked nil)
      )))

;;
(defsubst dotemacs-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local dotemacs-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(defun dotemacs-modeline|enable-word-count ()
  "Enable word count."
  (setq dotemacs-modeline-enable-word-count t))
(add-hook 'text-mode-hook #'dotemacs-modeline|enable-word-count)

(dotemacs-modeline-def-modeline-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (dotemacs-modeline--active) mark-active)
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max)))))
         (concat (cond ((bound-and-true-p rectangle-mark-mode)
                        (let ((cols (abs (- (dotemacs-column reg-end)
                                            (dotemacs-column reg-beg)))))
                          (format "%dx%dB" lines cols)))
                       ((> lines 1)
                        (format "%dC %dL" (- (1+ reg-end) reg-beg) lines))
                       (t
                        (format "%dC" (- (1+ reg-end) reg-beg))))
                 (when dotemacs-modeline-enable-word-count
                   (format " %dW" (count-words reg-beg reg-end)))))
       'face 'dotemacs-modeline-highlight))))

(dotemacs-modeline-def-modeline-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))

(dotemacs-modeline-def-modeline-segment bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (dotemacs-modeline--make-xpm
       (face-background (if (dotemacs-modeline--active)
                            'dotemacs-modeline-bar
                          'dotemacs-modeline-inactive-bar)
                        nil t)
       dotemacs-modeline-height
       3)
    ""))

;;
;; misc-info

(dotemacs-modeline-def-modeline-segment misc-info
  (propertize (format-time-string " %H:%M ")
              'help-echo (format-time-string "%c")))

;;
;; position

;; Be compatible with Emacs 25.
(defvar-local dotemacs-modeline-column-zero-based
  (or (bound-and-true-p column-number-indicator-zero-based) t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar-local dotemacs-modeline-percent-position
  (or (bound-and-true-p mode-line-percent-position) '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(setq-default mode-line-position
              '((line-number-mode
                 (column-number-mode
                  (dotemacs-modeline-column-zero-based " %l:%c" " %l:%C")
                  " %l")
                 (column-number-mode (dotemacs-modeline-column-zero-based " :%c" " :%C")))
                (if dotemacs-modeline-percent-position (" " dotemacs-modeline-percent-position))
                (:eval (when (or line-number-mode column-number-mode dotemacs-modeline-percent-position) " "))))

(dotemacs-modeline-def-modeline-segment buffer-position
  "The buffer position information."
  '(" " mode-line-position " "))

;;
;; Mode lines

(dotemacs-modeline-def-modeline main
  (bar buffer-info buffer-position selection-info)
  (buffer-encoding major-mode vcs flycheck))

(dotemacs-modeline-def-modeline minimal
  (bar buffer-info)
  (buffer-encoding major-mode))

(dotemacs-modeline-def-modeline special
  (bar buffer-info-simple buffer-position selection-info)
  (buffer-encoding major-mode flycheck))

(dotemacs-modeline-def-modeline project
  (bar buffer-default-directory)
  (major-mode))

(dotemacs-modeline-def-modeline media
  (bar buffer-info-simple)
  (media-info major-mode))

;;
;; Hooks

(defun dotemacs-modeline-init ()
  "Initialize modeline."
  
  ;; This scratch buffer is already created and doesn't get a modeline. For the
  ;; love of Emacs, someone give the man a modeline!
  (dolist (bname '("*scratch*" "*Messages*"))
    (with-current-buffer bname
      (dotemacs-set-modeline 'special))))

(defun dotemacs-modeline|set-special-modeline ()
  "Set sepcial mode-line."
  (dotemacs-set-modeline 'special))

(defun dotemacs-modeline|set-media-modeline ()
  "Set media mode-line."
  (dotemacs-set-modeline 'media))

(defun dotemacs-modeline|set-project-modeline ()
  "Set project mode-line."
  (dotemacs-set-modeline 'project))

;;
;; Bootstrap

(dotemacs-set-modeline 'main t)  ;; set default modeline
(add-hook 'dotemacs-load-theme-hook #'dotemacs-modeline-init)
;; (add-hook 'dotemacs-scratch-buffer-hook #'dotemacs-modeline|set-special-modeline)
;; (add-hook 'dotemacs-dashboard-mode-hook #'dotemacs-modeline|set-project-modeline)

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar dotemacs-modeline-remap-face-cookie nil)
(defun dotemacs-modeline|focus ()
  "Modeline focus hook."
  (when dotemacs-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative dotemacs-modeline-remap-face-cookie)))
(defun dotemacs-modeline|unfocus ()
  "Modeline unfocus hook."
  (setq dotemacs-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(add-hook 'focus-in-hook #'dotemacs-modeline|focus)
(add-hook 'focus-out-hook #'dotemacs-modeline|unfocus)

;;; config.el ends here
