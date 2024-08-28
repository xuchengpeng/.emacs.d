;;; init-modeline.el --- modeline. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup dotemacs-modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line)

(defgroup dotemacs-modeline-faces nil
  "The faces of `dotemacs-modeline'."
  :group 'dotemacs-modeline
  :group 'faces)

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

(defface dotemacs-modeline-panel
  '((t (:inherit dotemacs-modeline-highlight)))
  "Face for \\='X out of Y\\=' segments."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-info
  '((t (:inherit (dotemacs-modeline success))))
  "Face for info-level messages in the mode-line."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-warning
  '((t (:inherit (dotemacs-modeline warning))))
  "Face for warnings in the mode-line."
  :group 'dotemacs-modeline-faces)

(defface dotemacs-modeline-error
  '((t (:inherit (dotemacs-modeline error))))
  "Face for errors in the mode-line."
  :group 'dotemacs-modeline-faces)

(defvar eglot--mode-line-format)
(defvar flymake--state)
(defvar text-scale-mode-amount)
(defvar text-scale-mode-lighter)

(declare-function aw-update "ext:ace-window")
(declare-function flymake--diag-type "ext:flymake" t t)
(declare-function flymake--lookup-type-property "ext:flymake")
(declare-function flymake--state-diags "ext:flymake" t t)
(declare-function flymake-disabled-backends "ext:flymake")
(declare-function flymake-goto-next-error "ext:flymake")
(declare-function flymake-goto-prev-error "ext:flymake")
(declare-function flymake-reporting-backends "ext:flymake")
(declare-function flymake-running-backends "ext:flymake")
(declare-function flymake-show-buffer-diagnostics "ext:flymake")
(declare-function mc/num-cursors "ext:multiple-cursors-core")
(declare-function symbol-overlay-assoc "ext:symbol-overlay")
(declare-function symbol-overlay-get-list "ext:symbol-overlay")
(declare-function symbol-overlay-get-symbol "ext:symbol-overlay")
(declare-function warning-numeric-level "warnings")

(defun dotemacs-modeline--face (&optional face)
  "Display FACE in the selected window."
  (if (mode-line-window-selected-p)
      (or (and (facep face) `(:inherit (dotemacs-modeline ,face)))
          '(:inherit (dotemacs-modeline mode-line-active)))
    (or (and (facep face) `(:inherit (dotemacs-modeline mode-line-inactive ,face)))
        '(:inherit (dotemacs-modeline mode-line-inactive)))))

(defun dotemacs-modeline--window-number ()
  "Window number in mode-line."
  (let ((num (cond
              ((boundp 'ace-window-display-mode)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              (t ""))))
    (when (length> num 0)
      (concat
       " "
       (propertize
        num
        'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-major-mode))
       " "))))

(defun dotemacs-modeline--workspace-name ()
  "Workspace name in mode-line."
  (when (and (fboundp 'tab-bar-mode)
             (length> (frame-parameter nil 'tabs) 1))
    (let* ((current-tab (tab-bar--current-tab))
           (tab-index (tab-bar--current-tab-index))
           (explicit-name (alist-get 'explicit-name current-tab))
           (tab-name (alist-get 'name current-tab)))
      (concat
       " "
       (propertize
        (format "[%s]" (if explicit-name tab-name (+ 1 tab-index)))
        'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-major-mode))
       " "))))

(defun dotemacs-modeline--buffer-default-directory ()
  "Display `default-directory'."
  (concat
   " "
   (propertize
    (abbreviate-file-name default-directory)
    'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-path))
   " "))

(defun dotemacs-modeline--buffer-simple-name ()
  "Buffer simple name in mode-line."
  (propertize
   "%b"
   'face (if (buffer-modified-p)
             (dotemacs-modeline--face 'dotemacs-modeline-buffer-modified)
           (dotemacs-modeline--face 'dotemacs-modeline-buffer-file))))

(defun dotemacs-modeline--project-root ()
  "Get project root directory."
  (when-let ((project (project-current)))
    (expand-file-name
     (if (fboundp 'project-root)
         (project-root project)
       (car (with-no-warnings
              (project-roots project)))))))

(defun dotemacs-modeline--project-directory ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (abbreviate-file-name
   (or (dotemacs-modeline--project-root) default-directory)))

(defun dotemacs-modeline--buffer-name ()
  "Buffer name in mode-line."
  (let* ((root-path (file-local-name (dotemacs-modeline--project-directory)))
         (file-path (file-local-name (or (buffer-file-name (buffer-base-buffer)) ""))))
    (concat
     ;; Project directory
     (propertize (concat (file-name-nondirectory (directory-file-name root-path)) "/")
                 'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-path))
     ;; Relative path
     (propertize
      (when-let (relative-path (file-relative-name
                                (or (file-name-directory file-path) "./")
                                root-path))
        (if (string= relative-path "./")
            ""
          (if (<= (length relative-path) 15)
              relative-path
            ;; Shrink path
            (let ((split (string-split relative-path "/" 'omit-nulls)))
              (concat
               (mapconcat
                (lambda (str)
                  ;; Return first character or first two characters if hidden
                  (substring str 0 (if (string-prefix-p "." str) 2 1)))
                split
                "/")
               "/")))))
      'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-path))
     ;; File name
     (propertize
      (file-name-nondirectory file-path)
      'face (if (buffer-modified-p)
                (dotemacs-modeline--face 'dotemacs-modeline-buffer-modified)
              (dotemacs-modeline--face 'dotemacs-modeline-buffer-file))
      'help-echo (format "Buffer name\n%s" file-path)
      'mouse-face 'dotemacs-modeline-highlight))))

(defun dotemacs-modeline--buffer-info ()
  "Buffer info in mode-line."
  (concat
   " "
   (if buffer-file-name
       (dotemacs-modeline--buffer-name)
     (dotemacs-modeline--buffer-simple-name))
   " %I "))

(defun dotemacs-modeline--position ()
  "Position in mode-line."
  (concat
   " "
   (propertize "%l:%c %p%%"
               'help-echo "Buffer position"
               'mouse-face 'dotemacs-modeline-highlight)
   " "))

(defun dotemacs-modeline--word-count ()
  "Word count in mode-line."
  (when (member major-mode '(markdown-mode gfm-mode org-mode))
    (format " %dW " (count-words (point-min) (point-max)))))

(defun dotemacs-modeline--selection-info ()
  "Selection info in mode-line."
  (when (and mark-active (mode-line-window-selected-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (lines (count-lines beg (min end (point-max)))))
      (propertize
       (concat
        " "
        (cond ((bound-and-true-p rectangle-mark-mode)
               (let ((cols (abs (- (save-excursion (goto-char end) (current-column))
                                   (save-excursion (goto-char beg) (current-column))))))
                 (format "%dx%dB" lines cols)))
              ((> lines 1)
               (format "%dC %dL" (- end beg) lines))
              (t
               (format "%dC" (- end beg))))
        (format " %dW" (count-words beg end))
        " ")
       'face 'dotemacs-modeline-emphasis))))

(defun dotemacs-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (when (symbol-overlay-assoc symbol)
        (propertize
         (format " %d/%d " (+ count 1) (+ count (length after)))
         'face 'dotemacs-modeline-panel)))))

(defun dotemacs-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (when-let ((count (mc/num-cursors)))
      (propertize (format " MC:%d " count)
                  'face 'dotemacs-modeline-panel))))

(defun dotemacs-modeline--matches ()
  "Matches in mode-line."
  (when (mode-line-window-selected-p)
    (let ((meta (concat (dotemacs-modeline--symbol-overlay)
                        (dotemacs-modeline--multiple-cursors))))
      (unless (string-empty-p meta)
        meta))))

(defun dotemacs-modeline--buffer-encoding ()
  "Buffer encoding in mode-line."
  (concat
   " "
   (pcase (coding-system-eol-type buffer-file-coding-system)
     (0 "LF ")
     (1 "CRLF ")
     (2 "CR ")
     (_ ""))
   (let ((sys (coding-system-plist buffer-file-coding-system)))
     (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
            "UTF-8")
           (t (upcase (symbol-name (plist-get sys :name))))))
   " "))

(defun dotemacs-modeline--text-scale ()
  "Text-Scale info in mode-line."
  (when (and (boundp 'text-scale-mode-lighter) (/= text-scale-mode-amount 0))
    (concat
     " "
     (propertize
      (format "(%s)" text-scale-mode-lighter)
      'mouse-face 'dotemacs-modeline-highlight
      'help-echo (concat "Text scale " text-scale-mode-lighter))
     " ")))

(defun dotemacs-modeline--eglot ()
  "Eglot in mode-line."
  (when (bound-and-true-p eglot--managed-mode)
    (concat
     " "
     (propertize
      (format-mode-line eglot--mode-line-format)
      'face (dotemacs-modeline--face 'eglot-mode-line))
     " ")))

(defun dotemacs-modeline--major-mode ()
  "Major mode in mode-line."
  (concat
   " "
   (propertize
    (format-mode-line mode-name)
    'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-major-mode)
    'help-echo "Major mode
mouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes"
    'mouse-face 'dotemacs-modeline-highlight
    'local-map mode-line-major-mode-keymap)
   " "))

(defun dotemacs-modeline--vc-info ()
  "Version control info in mode-line."
  (let ((meta (string-trim (format-mode-line '(vc-mode vc-mode)))))
    (unless (string-empty-p meta)
      (concat
       " "
       (propertize meta 'face `(:inherit (,(dotemacs-modeline--face 'dotemacs-modeline-info) bold)))
       " "))))

(defun dotemacs-modeline--flymake ()
  "Flymake in mode-line."
  (when (bound-and-true-p flymake-mode)
    (let* ((known (hash-table-keys flymake--state))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (all-disabled (and disabled (null running)))
           (some-waiting (cl-set-difference running reported))
           (warning-level (warning-numeric-level :warning))
           (debug-level (warning-numeric-level :debug))
           (.error 0)
           (.warning 0)
           (.info 0))
      (maphash (lambda (_b state)
                 (cl-loop
                  with diags = (flymake--state-diags state)
                  for diag in diags do
                  (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                 (warning-numeric-level :error))))
                    (cond ((> severity warning-level) (cl-incf .error))
                          ((> severity debug-level) (cl-incf .warning))
                          (t (cl-incf .info))))))
               flymake--state)
      (when-let ((text
                  (cond
                   (some-waiting nil)
                   ((null known) nil)
                   (all-disabled nil)
                   (t (let ((num (+ .error .warning .info)))
                        (when (> num 0)
                          (format "%s/%s/%s"
                                  (propertize
                                   (number-to-string .error)
                                   'face (dotemacs-modeline--face 'dotemacs-modeline-error))
                                  (propertize
                                   (number-to-string .warning)
                                   'face (dotemacs-modeline--face 'dotemacs-modeline-warning))
                                  (propertize
                                   (number-to-string .info)
                                   'face (dotemacs-modeline--face 'dotemacs-modeline-info)))))))))
        (concat
         " "
         (propertize
          text
          'help-echo
          (format "Flymake
error:%d, warning:%d, info:%d
mouse-1: Next error
mouse-2: Show all errors
mouse-3: Previous error"
                  .error .warning .info)
          'mouse-face 'dotemacs-modeline-highlight
          'local-map (let ((map (make-sparse-keymap)))
                       (keymap-set map "<mode-line> <mouse-1>" #'flymake-goto-next-error)
                       (keymap-set map "<mode-line> <mouse-2>" #'flymake-show-buffer-diagnostics)
                       (keymap-set map "<mode-line> <mouse-3>" #'flymake-goto-prev-error)
                       map))
         " ")))))

(defcustom dotemacs-modeline-left
  '(dotemacs-modeline--window-number
    dotemacs-modeline--workspace-name
    dotemacs-modeline--matches
    dotemacs-modeline--buffer-info
    dotemacs-modeline--position
    dotemacs-modeline--word-count
    dotemacs-modeline--selection-info)
  "List of items on the left of mode-line."
  :type '(list function)
  :group 'dotemacs-modeline)

(defcustom dotemacs-modeline-right
  '(dotemacs-modeline--text-scale
    dotemacs-modeline--buffer-encoding
    dotemacs-modeline--eglot
    dotemacs-modeline--major-mode
    dotemacs-modeline--vc-info
    dotemacs-modeline--flymake)
  "List of items on the right of mode-line."
  :type '(list function)
  :group 'dotemacs-modeline)

(defun dotemacs-modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(defun dotemacs-modeline--format (left-segments right-segments)
  "Return a string from LEFT-SEGMENTS and RIGHT-SEGMENTS."
  (let* ((left-str (dotemacs-modeline--format-segments left-segments))
         (right-str (dotemacs-modeline--format-segments right-segments)))
    (concat
     left-str
     (propertize
      " "
      'display
      `(space :align-to (- (+ right right-fringe right-margin scroll-bar)
                           ,(* (string-width right-str) 1.0))))
     right-str)))

(defun dotemacs-modeline--enable ()
  "Enable dotemacs-modeline."
  (setq-default mode-line-format
                '((:eval (dotemacs-modeline--format
                          dotemacs-modeline-left
                          dotemacs-modeline-right)))))

(defun dotemacs-modeline--disable ()
  "Disable dotemacs-modeline."
  (setq-default mode-line-format nil))

(define-minor-mode dotemacs-modeline-mode
  "Toggle `dotemacs-modeline' on or off."
  :group 'dotemacs-modeline
  :global t
  :lighter nil
  :keymap nil
  (if dotemacs-modeline-mode
      (dotemacs-modeline--enable)
    (dotemacs-modeline--disable)))

(provide 'init-modeline)
;;; init-modeline.el ends here
