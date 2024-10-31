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

(defvar eglot-menu)
(defvar eglot-menu-string)
(defvar eglot-server-menu)
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
(declare-function warning-numeric-level "warnings")

(defun dotemacs-modeline--face (&optional face)
  "Display FACE in the selected window."
  (if (mode-line-window-selected-p)
      (or (and (facep face) `(:inherit (dotemacs-modeline ,face)))
          '(:inherit (dotemacs-modeline mode-line-active)))
    (or (and (facep face) `(:inherit (dotemacs-modeline mode-line-inactive ,face)))
        '(:inherit (dotemacs-modeline mode-line-inactive)))))

(defsubst dotemacs-modeline--spc ()
  "Whitespace."
  (propertize " " 'face (dotemacs-modeline--face)))

(defun dotemacs-modeline--window-number ()
  "Window number in mode-line."
  (let ((num (cond
              ((boundp 'ace-window-display-mode)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              (t ""))))
    (when (length> num 0)
      (concat
       (dotemacs-modeline--spc)
       (propertize
        num
        'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-major-mode))
       (dotemacs-modeline--spc)))))

(defun dotemacs-modeline--workspace-name ()
  "Workspace name in mode-line."
  (when (and (fboundp 'tab-bar-mode)
             (length> (frame-parameter nil 'tabs) 1))
    (let* ((current-tab (tab-bar--current-tab))
           (tab-index (tab-bar--current-tab-index))
           (explicit-name (alist-get 'explicit-name current-tab))
           (tab-name (alist-get 'name current-tab)))
      (concat
       (dotemacs-modeline--spc)
       (propertize
        (format "[%s]" (if explicit-name tab-name (+ 1 tab-index)))
        'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-major-mode))
       (dotemacs-modeline--spc)))))

(defun dotemacs-modeline--buffer-default-directory ()
  "Display `default-directory'."
  (concat
   (dotemacs-modeline--spc)
   (propertize
    (abbreviate-file-name default-directory)
    'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-path))
   (dotemacs-modeline--spc)))

(defun dotemacs-modeline--buffer-info ()
  "Buffer info in mode-line."
  (concat
   (dotemacs-modeline--spc)
   (propertize
   "%b"
   'face (if (buffer-modified-p)
             (dotemacs-modeline--face 'dotemacs-modeline-buffer-modified)
           (dotemacs-modeline--face 'dotemacs-modeline-buffer-file))
   'help-echo (format "Buffer name\n%s" (or buffer-file-name ""))
   'mouse-face 'dotemacs-modeline-highlight)
   (propertize " %I " 'face (dotemacs-modeline--face))))

(defun dotemacs-modeline--position ()
  "Position in mode-line."
  (concat
   (dotemacs-modeline--spc)
   (propertize "%l:%c %p%%"
               'face (dotemacs-modeline--face)
               'help-echo "Buffer position"
               'mouse-face 'dotemacs-modeline-highlight)
   (dotemacs-modeline--spc)))

(defun dotemacs-modeline--word-count ()
  "Word count in mode-line."
  (when (member major-mode '(text-mode markdown-mode gfm-mode org-mode))
    (propertize
     (format " %dW " (count-words (point-min) (point-max)))
     'face (dotemacs-modeline--face))))

(defun dotemacs-modeline--buffer-encoding ()
  "Buffer encoding in mode-line."
  (propertize
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
    " ")
   'face (dotemacs-modeline--face)))

(defun dotemacs-modeline--text-scale ()
  "Text-Scale info in mode-line."
  (when (and (boundp 'text-scale-mode-lighter) (/= text-scale-mode-amount 0))
    (concat
     (dotemacs-modeline--spc)
     (propertize
      (format "(%s)" text-scale-mode-lighter)
      'face (dotemacs-modeline--face)
      'mouse-face 'dotemacs-modeline-highlight
      'help-echo (concat "Text scale " text-scale-mode-lighter))
     (dotemacs-modeline--spc))))

(defun dotemacs-modeline--eglot ()
  "Eglot in mode-line."
  (when (bound-and-true-p eglot--managed-mode)
    (concat
     (dotemacs-modeline--spc)
     (propertize
      eglot-menu-string
      'face (dotemacs-modeline--face 'eglot-mode-line)
      'help-echo "Eglot
mouse-1: Eglot Menu
mouse-3: Eglot Server Menu"
      'mouse-face 'dotemacs-modeline-highlight
      'local-map (let ((map (make-sparse-keymap)))
                   (keymap-set map "<mode-line> <mouse-1>" eglot-menu)
                   (keymap-set map "<mode-line> <mouse-3>" eglot-server-menu)
                   map))
     (dotemacs-modeline--spc))))

(defun dotemacs-modeline--major-mode ()
  "Major mode in mode-line."
  (concat
   (dotemacs-modeline--spc)
   (propertize
    (format-mode-line mode-name)
    'face (dotemacs-modeline--face 'dotemacs-modeline-buffer-major-mode)
    'help-echo "Major mode
mouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes"
    'mouse-face 'dotemacs-modeline-highlight
    'local-map mode-line-major-mode-keymap)
   (dotemacs-modeline--spc)))

(defun dotemacs-modeline--vc-info ()
  "Version control info in mode-line."
  (let ((meta (string-trim (format-mode-line '(vc-mode vc-mode)))))
    (unless (string-empty-p meta)
      (concat
       (dotemacs-modeline--spc)
       (propertize meta 'face `(:inherit (,(dotemacs-modeline--face 'dotemacs-modeline-info) bold)))
       (dotemacs-modeline--spc)))))

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
      (when-let* ((text
                   (cond
                    (some-waiting nil)
                    ((null known) nil)
                    (all-disabled nil)
                    (t (let ((num (+ .error .warning .info)))
                         (when (> num 0)
                           (concat (propertize
                                    (number-to-string .error)
                                    'face (dotemacs-modeline--face 'dotemacs-modeline-error))
                                   (propertize "/" 'face (dotemacs-modeline--face))
                                   (propertize
                                    (number-to-string .warning)
                                    'face (dotemacs-modeline--face 'dotemacs-modeline-warning))
                                   (propertize "/" 'face (dotemacs-modeline--face))
                                   (propertize
                                    (number-to-string .info)
                                    'face (dotemacs-modeline--face 'dotemacs-modeline-info)))))))))
        (concat
         (dotemacs-modeline--spc)
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
         (dotemacs-modeline--spc))))))

(defcustom dotemacs-modeline-left
  '(dotemacs-modeline--window-number
    dotemacs-modeline--workspace-name
    dotemacs-modeline--buffer-info
    dotemacs-modeline--position
    dotemacs-modeline--word-count)
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
         (right-str (dotemacs-modeline--format-segments right-segments))
         (right-width (progn
                        (add-face-text-property 0 (length right-str) 'mode-line t right-str)
                        (string-pixel-width right-str))))
    (concat
     left-str
     (propertize
      " "
      'face (dotemacs-modeline--face)
      'display
      `(space :align-to (,(- (window-pixel-width)
                             (window-scroll-bar-width)
                             (window-right-divider-width)
                             (* (or (cdr (window-margins)) 1)
                                (frame-char-width))
                             right-width))))
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
