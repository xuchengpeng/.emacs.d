;;; ui/modeline/dotemacs-modeline.el --- modeline. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;
;; Customization
;;

(defgroup dotemacs-modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line)

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
  "Major modes in which to display word count continuously."
  :type '(repeat (symbol :tag "Major-Mode"))
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

(defface dotemacs-modeline-project-root-dir
  '((t (:inherit (dotemacs-modeline-emphasis bold))))
  "Face used for the project part of the mode-line buffer path."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-panel
  '((t (:inherit dotemacs-modeline-highlight)))
  "Face for \\='X out of Y\\=' segments.
This applies to `anzu', `evil-substitute', `iedit' etc."
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

(defface dotemacs-modeline-compilation
  '((t (:inherit dotemacs-modeline-warning :slant italic)))
  "Face for compilation progress."
  :group 'dotemacs-modeline-faces)

;;
;; Externals
;;

(declare-function face-remap-remove-relative "face-remap")

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

(defmacro dotemacs-modeline-def-segment (name &rest body)
  "Define a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "dotemacs-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           (add-to-list 'dotemacs-modeline-var-alist (cons name (car body)))
           `(add-to-list 'dotemacs-modeline-var-alist (cons ',name ',(car body))))
          (t
           (add-to-list 'dotemacs-modeline-fn-alist (cons name sym))
           `(progn
              (defun ,sym () ,docstring ,@body)
              (add-to-list 'dotemacs-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (unless (and (fboundp 'subr-native-elisp-p)
                                 (subr-native-elisp-p (symbol-function #',sym)))
                      (byte-compile #',sym)))))))))

(defun dotemacs-modeline--prepare-segments (segments)
  "Prepare mode-line `SEGMENTS'."
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (cdr (assq seg dotemacs-modeline-fn-alist)))
                    (push (list :eval (list it)) forms))
                   ((setq it (cdr (assq seg dotemacs-modeline-var-alist)))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defun dotemacs-modeline-def-modeline (name lhs &optional rhs)
  "Define a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `dotemacs-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`dotemacs-modeline-def-segment'.

Example:
  (dotemacs-modeline-def-modeline \\='minimal
    \\='(bar matches \" \" buffer-info)
    \\='(media-info major-mode))
  (dotemacs-modeline-set-modeline \\='minimal t)"
  (let ((sym (intern (format "dotemacs-modeline-format--%s" name)))
        (lhs-forms (dotemacs-modeline--prepare-segments lhs))
        (rhs-forms (dotemacs-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (list lhs-forms
              (propertize
               " "
               'face (dotemacs-modeline-face)
               'display `(space
                          :align-to
                          (- (+ right right-fringe right-margin scroll-bar)
                             ,(let ((rhs-str (format-mode-line (cons "" rhs-forms))))
                                (* (string-width rhs-str) 1.0)))))
              rhs-forms))
      (concat "Modeline:\n"
              (format "  %s\n  %s"
                      (prin1-to-string lhs)
                      (prin1-to-string rhs))))))
(put 'dotemacs-modeline-def-modeline 'lisp-indent-function 'defun)

(defun dotemacs-modeline (key)
  "Return a mode-line configuration associated with KEY (a symbol).
Throws an error if it doesn't exist."
  (let ((fn (intern-soft (format "dotemacs-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun dotemacs-modeline-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let ((modeline (dotemacs-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            mode-line-format)
          (list "%e" modeline))))

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

(defvar evil-mc-frozen)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar flycheck-current-errors)
(defvar iedit-occurrences-overlays)
(defvar symbol-overlay-keywords-alist)
(defvar symbol-overlay-temp-symbol)
(defvar text-scale-mode-amount)
(defvar winum-auto-setup-mode-line)

(declare-function compilation-goto-in-progress-buffer "compile")
(declare-function flycheck-count-errors "ext:flycheck")
(declare-function flycheck-list-errors "ext:flycheck")
(declare-function flycheck-next-error "ext:flycheck")
(declare-function flycheck-previous-error "ext:flycheck")
(declare-function iedit-find-current-occurrence-overlay "ext:iedit-lib")
(declare-function iedit-prev-occurrence "ext:iedit-lib")
(declare-function mc/num-cursors "ext:multiple-cursors-core")
(declare-function projectile-project-root "ext:projectile")
(declare-function symbol-overlay-assoc "ext:symbol-overlay")
(declare-function symbol-overlay-get-list "ext:symbol-overlay")
(declare-function symbol-overlay-get-symbol "ext:symbol-overlay")
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
  (propertize "%b"
              'face (dotemacs-modeline-face
                    (if (buffer-modified-p)
                        'dotemacs-modeline-buffer-modified
                      'dotemacs-modeline-buffer-file))))

(dotemacs-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat
   (dotemacs-modeline-spc)
   (dotemacs-modeline--buffer-simple-name)
   (dotemacs-modeline-spc)))

(defun dotemacs-modeline--project-root ()
  "Get project root directory."
  (when (bound-and-true-p projectile-mode)
    (projectile-project-root)))

(defsubst dotemacs-modeline--buffer-name ()
  "The buffer name."
  (let ((file-path (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
        (project-root (or (dotemacs-modeline--project-root) default-directory)))
    (concat
      ;; Project directory
      (propertize (concat (file-name-nondirectory (directory-file-name project-root)) "/")
                  'face (dotemacs-modeline-face 'dotemacs-modeline-project-root-dir))
      ;; Relative path
      (propertize
        (when-let (relative-path (file-relative-name
                                  (or (file-name-directory file-path) "./")
                                  project-root))
          (if (string= relative-path "./")
              ""
            relative-path))
        'face (dotemacs-modeline-face 'dotemacs-modeline-buffer-path))
      ;; File name
      (propertize (file-name-nondirectory file-path)
                  'face (dotemacs-modeline-face (if (buffer-modified-p)
                                                    'dotemacs-modeline-buffer-modified
                                                  'dotemacs-modeline-buffer-file))))))

(dotemacs-modeline-def-segment buffer-info
  "Display buffer info."
  (concat
    (dotemacs-modeline-spc)
    (if buffer-file-name
        (dotemacs-modeline--buffer-name)
      (dotemacs-modeline--buffer-simple-name))
    (dotemacs-modeline-spc)))

(dotemacs-modeline-def-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (concat
    (dotemacs-modeline-spc)
    (propertize (abbreviate-file-name default-directory)
                'face (dotemacs-modeline-face 'dotemacs-modeline-buffer-path))
    (dotemacs-modeline-spc)))

(dotemacs-modeline-def-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (dotemacs-modeline-spc)
          (pcase (coding-system-eol-type buffer-file-coding-system)
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
  (when-let (position '("" "%l:%c" " " (-3 "%p") "%%"))
    (concat
      (dotemacs-modeline-spc)
      (propertize (format-mode-line position)
                  'face (dotemacs-modeline-face)
                  'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                  'mouse-face 'dotemacs-modeline-highlight
                  'local-map mode-line-column-line-number-mode-map)
      (dotemacs-modeline-spc))))

(dotemacs-modeline-def-segment word-count
  "The buffer word count."
  (when (member major-mode dotemacs-modeline-continuous-word-count-modes)
    (propertize (format " %dW " (count-words (point-min) (point-max)))
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
  (when (dotemacs-modeline--active)
    (dotemacs-modeline-display-text
      (concat
        (dotemacs-modeline-spc)
        (format-mode-line mode-line-misc-info)
        (dotemacs-modeline-spc)))))

(dotemacs-modeline-def-segment minor-modes
  (when dotemacs-modeline-minor-modes
    (let ((face (dotemacs-modeline-face 'dotemacs-modeline-buffer-minor-mode))
          (mouse-face 'dotemacs-modeline-highlight)
          (help-echo "Minor mode
  mouse-1: Display minor mode menu
  mouse-2: Show help for minor mode
  mouse-3: Toggle minor modes"))
      `((:propertize ("" minor-mode-alist)
          face ,face
          mouse-face ,mouse-face
          help-echo ,help-echo
          local-map ,mode-line-minor-mode-keymap)
        ,(dotemacs-modeline-spc)))))

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
    (when-let (text
      (pcase flycheck-last-status-change
        (`finished
          (when flycheck-current-errors
            (let-alist (flycheck-count-errors flycheck-current-errors)
              (let ((error (or .error 0))
                    (warning (or .warning 0))
                    (info (or .info 0)))
                (format " %s %s %s "
                        (propertize (concat "E" (number-to-string error))
                                    'face (dotemacs-modeline-face 'dotemacs-modeline-urgent))
                        (propertize (concat "W" (number-to-string warning))
                                    'face (dotemacs-modeline-face 'dotemacs-modeline-warning))
                        (propertize (concat "I" (number-to-string info))
                                    'face (dotemacs-modeline-face 'dotemacs-modeline-info)))))))
        (`running nil)
        (`not-checked nil)
        (`errored (propertize " Error " 'face (dotemacs-modeline-face 'dotemacs-modeline-urgent)))
        (`interrupted (propertize " Interrupted " 'face (dotemacs-modeline-face 'dotemacs-modeline-warning)))
        (`suspicious '(propertize " Suspicious " 'face (dotemacs-modeline-face 'dotemacs-modeline-urgent)))))
      (propertize text
                  'mouse-face 'dotemacs-modeline-highlight
                  'help-echo "Flycheck
  mouse-1: Next error
  mouse-2: Show all errors
  mouse-3: Previous error"
                  'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line mouse-1] #'flycheck-next-error)
                        (define-key map [mode-line mouse-2] #'flycheck-list-errors)
                        (define-key map [mode-line mouse-3] #'flycheck-previous-error)
                        map)))))

(defun dotemacs-modeline-themes--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defsubst dotemacs-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion (iedit-prev-occurrence)
                                        (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'dotemacs-modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (dotemacs-modeline-face 'dotemacs-modeline-panel))))

(defsubst dotemacs-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when (and (dotemacs-modeline--active)
             (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol))
             (not (bound-and-true-p iedit-mode)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (propertize
           (format (concat  " %d/%d " (and (cadr keyword) "in scope "))
                   (+ count 1)
                   (+ count (length after)))
           'face (dotemacs-modeline-face 'dotemacs-modeline-panel))))))

(defsubst dotemacs-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (cl-destructuring-bind (count . face)
    (cond ((bound-and-true-p multiple-cursors-mode)
           (cons (mc/num-cursors)
                 (dotemacs-modeline-face 'dotemacs-modeline-panel)))
          ((bound-and-true-p evil-mc-cursor-list)
           (cons (length evil-mc-cursor-list)
                 (dotemacs-modeline-face (if evil-mc-frozen
                                         'dotemacs-modeline-bar
                                       'dotemacs-modeline-panel))))
          ((cons nil nil)))
    (when count
      (propertize (format " MC:%d " count) 'face face))))

(defsubst dotemacs-modeline--buffer-size ()
  "Show buffer size."
  (when buffer-file-name
    (concat (dotemacs-modeline-spc)
            (propertize "%I"
                        'face (dotemacs-modeline-face)
                        'help-echo "Buffer size
  mouse-1: Display Line and Column Mode Menu"
                        'mouse-face 'dotemacs-modeline-highlight
                        'local-map mode-line-column-line-number-mode-map)
            (dotemacs-modeline-spc))))

(dotemacs-modeline-def-segment matches
  "Displays matches."
  (let ((meta (concat (dotemacs-modeline--iedit)
                      (dotemacs-modeline--symbol-overlay)
                      (dotemacs-modeline--multiple-cursors))))
    (or (and (not (string-empty-p meta)) meta)
        (dotemacs-modeline--buffer-size))))

(dotemacs-modeline-def-segment compilation
  (when (bound-and-true-p compilation-in-progress)
    (propertize " [Compiling] "
                'face (dotemacs-modeline-face 'dotemacs-modeline-compilation)
                'help-echo "Compiling; mouse-2: Goto Buffer"
                'mouse-face 'dotemacs-modeline-highlight
                'local-map (make-mode-line-mouse-map 'mouse-2 #'compilation-goto-in-progress-buffer))))

;;
;; Modelines
;;

(dotemacs-modeline-def-modeline 'main
  '(bar window-number matches buffer-info buffer-position word-count selection-info)
  '(compilation misc-info minor-modes buffer-encoding major-mode process vcs checker))

(dotemacs-modeline-def-modeline 'special
  '(bar window-number matches buffer-info-simple buffer-position)
  '(compilation misc-info major-mode process))

(dotemacs-modeline-def-modeline 'project
  '(bar window-number buffer-default-directory buffer-position)
  '(compilation misc-info major-mode process))

(dotemacs-modeline-def-modeline 'dashboard
  '(bar window-number buffer-default-directory)
  '(compilation misc-info major-mode process))

(defun dotemacs-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (dotemacs-modeline-set-modeline 'main default))

(dotemacs-modeline-set-main-modeline t)

(defvar dotemacs-modeline-mode-alist
  '((message-mode . special)
    (circe-mode . special)
    (erc-mode . special)
    (rcirc-mode . special)
    (Info-mode .  special))
  "Alist of major modes and mode-lines.")

(defun dotemacs-modeline-auto-set-modeline ()
  "Set mode-line base on major-mode."
  (catch 'found
    (dolist (x dotemacs-modeline-mode-alist)
      (when (derived-mode-p (car x))
        (dotemacs-modeline-set-modeline (cdr x))
        (throw 'found x)))))

(add-hook 'after-change-major-mode-hook #'dotemacs-modeline-auto-set-modeline)

(provide 'dotemacs-modeline)
;;; dotemacs-modeline.el ends here
