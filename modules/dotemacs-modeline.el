;;; ui/modeline/dotemacs-modeline.el --- modeline.

;;; Commentary:
;;
;; dotemacs modeline.
;; 

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;
;; Customization
;;

(defgroup dotemacs-modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/dotemacs-modeline"))

(defcustom dotemacs-modeline-height 23
  "How tall the mode-line should be. It's only respected in GUI.
If the actual char height is larger, it respects the actual char height."
  :type 'integer
  :group 'dotemacs-modeline)

(defcustom dotemacs-modeline-bar-width 4
  "How wide the mode-line bar should be. It's only respected in GUI."
  :type 'integer
  :set (lambda (sym val)
         (set sym (if (> val 0) val 1)))
  :group 'dotemacs-modeline)

(defcustom dotemacs-modeline-continuous-word-count-modes
  '(markdown-mode gfm-mode org-mode)
  "Major modes in which to display word count continuously.

It respects `dotemacs-modeline-enable-word-count'."
  :type '(repeat (symbol :tag "Major-Mode") )
  :group 'dotemacs-modeline)

(defcustom dotemacs-modeline-minor-modes nil
  "Whether display the minor modes in the mode-line."
  :type 'boolean
  :group 'dotemacs-modeline)

(defcustom dotemacs-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control."
  :type 'integer
  :group 'dotemacs-modeline)

;;
;; Faces
;;

(defgroup dotemacs-modeline-faces nil
  "The faces of `dotemacs-modeline'."
  :group 'dotemacs-modeline
  :group 'faces
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/dotemacs-modeline"))

(defface dotemacs-modeline
  '((t ()))
  "Default face."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-emphasis
  '((t (:inherit (dotemacs-modeline mode-line-emphasis))))
  "Face used for emphasis."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-highlight
  '((t (:inherit (dotemacs-modeline mode-line-highlight))))
  "Face used for highlighting."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-buffer-path
  '((t (:inherit (dotemacs-modeline-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-buffer-file
  '((t (:inherit (dotemacs-modeline mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-buffer-modified
  '((t (:inherit (dotemacs-modeline warning bold) :background unspecified)))
  "Face used for the \\='unsaved\\=' symbol in the mode-line."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-buffer-major-mode
  '((t (:inherit (dotemacs-modeline-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-buffer-minor-mode
  '((t (:inherit (dotemacs-modeline font-lock-doc-face) :weight normal :slant normal)))
  "Face used for the minor-modes segment in the mode-line."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-project-parent-dir
  '((t (:inherit (dotemacs-modeline font-lock-comment-face bold))))
  "Face used for the project parent directory of the mode-line buffer path."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-project-dir
  '((t (:inherit (dotemacs-modeline font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-project-root-dir
  '((t (:inherit (dotemacs-modeline-emphasis bold))))
  "Face used for the project part of the mode-line buffer path."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-debug
  '((t (:inherit (dotemacs-modeline font-lock-doc-face) :slant normal)))
  "Face for debug-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-info
  '((t (:inherit (dotemacs-modeline success))))
  "Face for info-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-warning
  '((t (:inherit (dotemacs-modeline warning))))
  "Face for warnings in the mode-line. Used by vcs, checker, etc."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-urgent
  '((t (:inherit (dotemacs-modeline error))))
  "Face for errors in the mode-line. Used by vcs, checker, etc."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-notification
  '((t (:inherit dotemacs-modeline-warning)))
  "Face for notifications in the mode-line. Used by GitHub, mu4e, etc.
Also see the face `dotemacs-modeline-unread-number'."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-bar
  '((t (:inherit dotemacs-modeline-highlight)))
  "The face used for the left-most bar in the mode-line of an active window."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-bar-inactive
  `((t (:background ,(face-foreground 'mode-line-inactive))))
  "The face used for the left-most bar in the mode-line of an inactive window."
  :group 'dotemacs-modeline-faces)

;;
;; Externals
;;

(declare-function face-remap-remove-relative "face-remap")
(declare-function ffip-project-root "ext:find-file-in-project")
(declare-function project-root "project")
(declare-function projectile-project-root "ext:projectile")

;; Keep `dotemacs-modeline-current-window' up-to-date
(defun dotemacs-modeline--selected-window ()
  "Get the selected window."
  (frame-selected-window))

(defvar dotemacs-modeline-current-window (dotemacs-modeline--selected-window)
  "Current window.")

(defun dotemacs-modeline--active ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and dotemacs-modeline-current-window
         (eq (dotemacs-modeline--selected-window) dotemacs-modeline-current-window))))

(defun dotemacs-modeline-set-selected-window (&rest _)
  "Set `dotemacs-modeline-current-window' appropriately."
  (let ((win (dotemacs-modeline--selected-window)))
    (setq dotemacs-modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun dotemacs-modeline-unset-selected-window ()
  "Unset `dotemacs-modeline-current-window' appropriately."
  (setq dotemacs-modeline-current-window nil))

(add-hook 'pre-redisplay-functions #'dotemacs-modeline-set-selected-window)

;; Ensure modeline is inactive when Emacs is unfocused
(defvar dotemacs-modeline--remap-faces '(mode-line
                                     mode-line-active
                                     mode-line-emphasis
                                     mode-line-highlight
                                     mode-line-buffer-id
                                     dotemacs-modeline
                                     solaire-mode-line-face
                                     solaire-mode-line-active-face
                                     paradox-mode-line-face
                                     flycheck-color-mode-line-error-face
                                     flycheck-color-mode-line-warning-face
                                     flycheck-color-mode-line-info-face
                                     flycheck-color-mode-line-success-face))

(defvar dotemacs-modeline--remap-face-cookie-alist nil)
(defun dotemacs-modeline-focus ()
  "Focus mode-line."
  (mapc #'face-remap-remove-relative dotemacs-modeline--remap-face-cookie-alist))

(defun dotemacs-modeline-unfocus ()
  "Unfocus mode-line."
  (dolist (face dotemacs-modeline--remap-faces)
    (add-to-list 'dotemacs-modeline--remap-face-cookie-alist
                 (face-remap-add-relative face 'mode-line-inactive))))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun dotemacs-modeline-focus-change (&rest _)
          (if (frame-focus-state (frame-parent))
              (progn
                (dotemacs-modeline-focus)
                ;; HACK: pulse after focusing in the frame to refresh the buffer name.
                ;; @see https://github.com/seagle0128/dotemacs-modeline/issues/591
                (when (fboundp 'pulse-momentary-highlight-region)
                  (pulse-momentary-highlight-region 0 0)))
            (dotemacs-modeline-unfocus)))
        (advice-add #'handle-switch-frame :after #'dotemacs-modeline-focus-change)
        (add-function :after after-focus-change-function #'dotemacs-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'dotemacs-modeline-focus)
      (add-hook 'focus-out-hook #'dotemacs-modeline-unfocus))))

;;
;; Core
;;

(defvar dotemacs-modeline-fn-alist ())
(defvar dotemacs-modeline-var-alist ())

(defmacro dotemacs-modeline-def-segment (name &rest forms)
  "Define a modeline segment and byte compiles it with NAME and FORMS."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "dotemacs-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst dotemacs-modeline--prepare-segments (segments)
  "Prepare modelin SEGMENTS."
  (cl-loop for seg in segments
           if (stringp seg)
           collect seg
           else
           collect (list (intern (format "dotemacs-modeline-segment--%s" (symbol-name seg))))))

(defmacro dotemacs-modeline-def-modeline (name lhs &optional rhs)
  "Define a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `dotemacs-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`dotemacs-modeline-def-modeline-segment'.
Example:
  (dotemacs-modeline-def-modeline minimal
    (bar \" \" buffer-info)
    (media-info major-mode))
  (dotemacs-set-modeline 'minimal t)"
  (let ((sym (intern (format "dotemacs-modeline-format--%s" name)))
        (lhs-forms (dotemacs-modeline--prepare-segments lhs))
        (rhs-forms (dotemacs-modeline--prepare-segments rhs)))
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
  (let ((fn (intern-soft (format "dotemacs-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun dotemacs-modeline-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let* ((modeline (dotemacs-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))


;;
;; Helpers
;;

(defsubst dotemacs-modeline-spc ()
  "Whitespace."
  (propertize " " 'face (dotemacs-modeline-face)))

(defun dotemacs-modeline-face (&optional face inactive-face)
  "Display FACE in active window, and INACTIVE-FACE in inactive window.
IF FACE is nil, `mode-line' face will be used.
If INACTIVE-FACE is nil, `mode-line-inactive' face will be used."
  (if (dotemacs-modeline--active)
      (or (and (facep face) face)
          (and (facep 'mode-line-active) 'mode-line-active)
          'mode-line)
    (or (and (facep face) `(:inherit (mode-line-inactive ,face)))
        (and (facep inactive-face) inactive-face)
        'mode-line-inactive)))

(defun dotemacs-modeline-display-text (text)
  "Display TEXT in mode-line."
  (if (dotemacs-modeline--active)
      text
    (propertize text 'face 'mode-line-inactive)))

(defun dotemacs-modeline--create-bar-image (face width height)
  "Create the bar image.

Use FACE for the bar, WIDTH and HEIGHT are the image size in pixels."
  (when (and (image-type-available-p 'pbm)
             (numberp width) (> width 0)
             (numberp height) (> height 0))
    (propertize
     " " 'display
     (let ((color (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (make-string (* width height) ?1)
                  "\n")
          'pbm t :scale 1 :foreground color :ascent 'center))))))

;;
;; Segments
;;

(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar flycheck-current-errors)
(defvar text-scale-mode-amount)
(defvar winum-auto-setup-mode-line)

(declare-function flycheck-count-errors "ext:flycheck")
(declare-function window-numbering-clear-mode-line "ext:window-numbering")
(declare-function window-numbering-get-number-string "ext:window-numbering")
(declare-function window-numbering-install-mode-line "ext:window-numbering")
(declare-function winum--clear-mode-line "ext:winum")
(declare-function winum--install-mode-line "ext:winum")
(declare-function winum-get-number-string "ext:winum")

(defvar dotemacs-modeline--bar-active nil)
(defvar dotemacs-modeline--bar-inactive nil)

(defsubst dotemacs-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and dotemacs-modeline--bar-active dotemacs-modeline--bar-inactive)
    (let ((width dotemacs-modeline-bar-width)
          (height dotemacs-modeline-height))
      (setq dotemacs-modeline--bar-active
            (dotemacs-modeline--create-bar-image 'dotemacs-modeline-bar width height)
            dotemacs-modeline--bar-inactive
            (dotemacs-modeline--create-bar-image
             'dotemacs-modeline-bar-inactive width height))))
  (if (dotemacs-modeline--active)
      dotemacs-modeline--bar-active
    dotemacs-modeline--bar-inactive))

(defun dotemacs-modeline-refresh-bars ()
  "Refresh mode-line bars on next redraw."
  (setq dotemacs-modeline--bar-active nil
        dotemacs-modeline--bar-inactive nil))

(add-hook 'window-configuration-change-hook #'dotemacs-modeline-refresh-bars)

(dotemacs-modeline-def-segment bar
  "The bar regulates the height of the `dotemacs-modeline' in GUI."
  (dotemacs-modeline--bar))

(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)
(advice-add #'winum--install-mode-line :override #'ignore)
(advice-add #'winum--clear-mode-line :override #'ignore)

(dotemacs-modeline-def-segment window-number
  "The current window number."
  (let ((num (cond
              ((bound-and-true-p winum-mode)
               (setq winum-auto-setup-mode-line nil)
               (winum-get-number-string))
              ((bound-and-true-p window-numbering-mode)
               (window-numbering-get-number-string))
              (t ""))))
    (if (length> num 0)
        (propertize (format " %s " num)
                    'face (dotemacs-modeline-face 'dotemacs-modeline-buffer-major-mode))
      (dotemacs-modeline-spc))))

(defsubst dotemacs-modeline--buffer-simple-name ()
  "The buffer simple name."
  (concat
    (if buffer-file-name "%I ")
    (propertize "%b"
                'face (dotemacs-modeline-face
                      (if (buffer-modified-p)
                          'dotemacs-modeline-buffer-modified
                        'dotemacs-modeline-buffer-file)))))

(dotemacs-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat
   (dotemacs-modeline-spc)
   (dotemacs-modeline--buffer-simple-name)))

(dotemacs-modeline-def-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-path)))
    (concat (dotemacs-modeline-spc)
            (propertize (abbreviate-file-name default-directory)
                        'face face))))

(dotemacs-modeline-def-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF ")
            (1 "CRLF ")
            (2 "CR ")
            (_ ""))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          (dotemacs-modeline-spc)))

(dotemacs-modeline-def-segment buffer-position
  "The buffer position information."
  '(" " "%2l:%c" " " (-3 "%p") " "))

(dotemacs-modeline-def-segment word-count
  "The buffer word count."
  (when (member major-mode dotemacs-modeline-continuous-word-count-modes)
    (propertize (format " %dW" (count-words (point-min) (point-max)))
                'face (dotemacs-modeline-face))))

(defsubst dotemacs-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(dotemacs-modeline-def-segment selection-info
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM dimensions of a
block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (dotemacs-modeline--active))
    (cl-destructuring-bind (beg . end)
      (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
          (cons evil-visual-beginning evil-visual-end)
        (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (dotemacs-modeline-spc)
                 (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (dotemacs-modeline-column end)
                                            (dotemacs-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       (t
                        (format "%dC" (- end beg))))
                 (format " %dW" (count-words beg end))
                 (dotemacs-modeline-spc)))
       'face 'dotemacs-modeline-emphasis))))

(dotemacs-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (dotemacs-modeline-display-text
     (format-mode-line mode-line-misc-info)))

(dotemacs-modeline-def-segment minor-modes
  (if dotemacs-modeline-minor-modes
      (let ((face (dotemacs-modeline-face 'dotemacs-modeline-buffer-minor-mode))
            (mouse-face 'dotemacs-modeline-highlight)
            (help-echo "Minor mode
    mouse-1: Display minor mode menu
    mouse-2: Show help for minor mode
    mouse-3: Toggle minor modes"))
        `((:propertize ("" minor-mode-alist)
            face ,face
            mouse-face ,mouse-face
            help-echo ,help-echo)
          ,(dotemacs-modeline-spc)))
    ""))

(dotemacs-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (dotemacs-modeline-spc)
    (propertize (format-mode-line
                 (or (and (boundp 'delighted-modes)
                          (cadr (assq major-mode delighted-modes)))
                     mode-name))
                'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                'mouse-face 'dotemacs-modeline-highlight
                'local-map mode-line-major-mode-keymap)
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (dotemacs-modeline-spc))
   'face (dotemacs-modeline-face 'dotemacs-modeline-buffer-major-mode)))

(dotemacs-modeline-def-segment process
  "The process info."
  (dotemacs-modeline-display-text
   (format-mode-line mode-line-process)))

(defvar-local dotemacs-modeline--vcs-text nil)
(defun dotemacs-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (setq dotemacs-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        ""))
                 (face (cond ((eq state 'needs-update)
                              'dotemacs-modeline-warning)
                             ((memq state '(removed conflict unregistered))
                              'dotemacs-modeline-urgent)
                             (t 'dotemacs-modeline-info))))
            (propertize (if (length> str dotemacs-modeline-vcs-max-length)
                            (concat
                             (substring str 0 (- dotemacs-modeline-vcs-max-length 3))
                             "...")
                          str)
                        'mouse-face 'dotemacs-modeline-highlight
                        'face `(:inherit (,face bold)))))))
(add-hook 'find-file-hook #'dotemacs-modeline-update-vcs-text)
(add-hook 'after-save-hook #'dotemacs-modeline-update-vcs-text)
(advice-add #'vc-refresh-state :after #'dotemacs-modeline-update-vcs-text)

(dotemacs-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (when-let (text dotemacs-modeline--vcs-text)
    (concat
     (dotemacs-modeline-spc)
     (propertize (dotemacs-modeline-display-text text)
                 'mouse-face 'dotemacs-modeline-highlight
                 'help-echo (get-text-property 1 'help-echo vc-mode)
                 'local-map (get-text-property 1 'local-map vc-mode))
     (dotemacs-modeline-spc))))

(dotemacs-modeline-def-segment checker
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      (`finished
       (when flycheck-current-errors
         (let-alist (flycheck-count-errors flycheck-current-errors)
           (let ((error (or .error 0))
                  (warning (or .warning 0))
                  (info (or .info 0)))
             (format " %s %s %s "
                     (propertize (concat "E" (number-to-string error)) 'face (dotemacs-modeline-face 'dotemacs-modeline-urgent))
                     (propertize (concat "W" (number-to-string warning)) 'face (dotemacs-modeline-face 'dotemacs-modeline-warning))
                     (propertize (concat "I" (number-to-string info)) 'face (dotemacs-modeline-face 'dotemacs-modeline-info)))))))
      (`running nil)
      (`not-checked nil)
      (`errored (propertize " Error " 'face (dotemacs-modeline-face 'dotemacs-modeline-urgent)))
      (`interrupted (propertize " Interrupted " 'face (dotemacs-modeline-face 'dotemacs-modeline-warning)))
      (`suspicious '(propertize " Suspicious " 'face (dotemacs-modeline-face 'dotemacs-modeline-urgent))))))

;;
;; Modelines
;;

(dotemacs-modeline-def-modeline main
  (bar window-number buffer-info-simple buffer-position word-count selection-info)
  (minor-modes buffer-encoding major-mode process vcs checker))

(defun dotemacs-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (dotemacs-modeline-set-modeline 'main default))

(dotemacs-modeline-set-main-modeline t)

(provide 'dotemacs-modeline)
;;; dotemacs-modeline.el ends here
