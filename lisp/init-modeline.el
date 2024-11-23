;;; init-modeline.el --- modeline. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup +modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line)

(defgroup +modeline-faces nil
  "The faces of `+modeline'."
  :group '+modeline
  :group 'faces)

(defface +modeline
  '((t ()))
  "Default face."
  :group '+modeline-faces)

(defface +modeline-emphasis
  '((t (:inherit (+modeline mode-line-emphasis))))
  "Face used for emphasis."
  :group '+modeline-faces)

(defface +modeline-highlight
  '((t (:inherit (+modeline mode-line-highlight))))
  "Face used for highlighting."
  :group '+modeline-faces)

(defface +modeline-buffer-path
  '((t (:inherit (+modeline-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+modeline-faces)

(defface +modeline-buffer-file
  '((t (:inherit (+modeline mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+modeline-faces)

(defface +modeline-buffer-modified
  '((t (:inherit (+modeline warning bold) :background unspecified)))
  "Face used for the \\='unsaved\\=' symbol in the mode-line."
  :group '+modeline-faces)

(defface +modeline-buffer-major-mode
  '((t (:inherit (+modeline-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+modeline-faces)

(defface +modeline-debug
  '((t (:inherit (+modeline success))))
  "Face for debug-level messages in the mode-line."
  :group '+modeline-faces)

(defface +modeline-warning
  '((t (:inherit (+modeline warning))))
  "Face for warnings in the mode-line."
  :group '+modeline-faces)

(defface +modeline-error
  '((t (:inherit (+modeline error))))
  "Face for errors in the mode-line."
  :group '+modeline-faces)

(defface +modeline-vc-info
  '((t (:inherit (+modeline success bold))))
  "Face for vc-info in the mode-line."
  :group '+modeline-faces)

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

(defun +modeline--face (&optional face)
  "Display FACE in the selected window."
  (if (mode-line-window-selected-p)
      (or (and (facep face) `(:inherit (+modeline ,face)))
          '(:inherit (+modeline mode-line-active)))
    (or (and (facep face) `(:inherit (+modeline mode-line-inactive ,face)))
        '(:inherit (+modeline mode-line-inactive)))))

(defsubst +modeline--spc ()
  "Whitespace."
  (propertize " " 'face (+modeline--face)))

(defun +modeline--window-number ()
  "Window number in mode-line."
  (let ((num (cond
              ((boundp 'ace-window-display-mode)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              (t ""))))
    (when (length> num 0)
      (concat
       (+modeline--spc)
       (propertize
        num
        'face (+modeline--face '+modeline-buffer-major-mode))
       (+modeline--spc)))))

(defun +modeline--workspace-name ()
  "Workspace name in mode-line."
  (when (and (fboundp 'tab-bar-mode)
             (length> (frame-parameter nil 'tabs) 1))
    (let* ((current-tab (tab-bar--current-tab))
           (tab-index (tab-bar--current-tab-index))
           (explicit-name (alist-get 'explicit-name current-tab))
           (tab-name (alist-get 'name current-tab)))
      (concat
       (+modeline--spc)
       (propertize
        (format "[%s]" (if explicit-name tab-name (+ 1 tab-index)))
        'face (+modeline--face '+modeline-buffer-major-mode))
       (+modeline--spc)))))

(defun +modeline--buffer-default-directory ()
  "Display `default-directory'."
  (concat
   (+modeline--spc)
   (propertize
    (abbreviate-file-name default-directory)
    'face (+modeline--face '+modeline-buffer-path))
   (+modeline--spc)))

(defun +modeline--buffer-info ()
  "Buffer info in mode-line."
  (concat
   (+modeline--spc)
   (propertize
    "%b"
    'face (if (and (buffer-modified-p) (not buffer-read-only))
              (+modeline--face '+modeline-buffer-modified)
            (+modeline--face '+modeline-buffer-file))
    'help-echo (format "Buffer name\n%s" (or (buffer-file-name) (buffer-name)))
    'mouse-face '+modeline-highlight)
   (+modeline--spc)
   (propertize "%I" 'face (+modeline--face))
   (+modeline--spc)))

(defun +modeline--position ()
  "Position in mode-line."
  (concat
   (+modeline--spc)
   (propertize "%l:%c %p%%"
               'face (+modeline--face)
               'help-echo "Buffer position"
               'mouse-face '+modeline-highlight)
   (+modeline--spc)))

(defun +modeline--word-count ()
  "Word count in mode-line."
  (when (member major-mode '(text-mode markdown-mode gfm-mode org-mode))
    (propertize
     (format " %dW " (count-words (point-min) (point-max)))
     'face (+modeline--face))))

(defun +modeline--buffer-encoding ()
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
   'face (+modeline--face)))

(defun +modeline--text-scale ()
  "Text-Scale info in mode-line."
  (when (and (boundp 'text-scale-mode-lighter) (/= text-scale-mode-amount 0))
    (concat
     (+modeline--spc)
     (propertize
      (format "(%s)" text-scale-mode-lighter)
      'face (+modeline--face)
      'mouse-face '+modeline-highlight
      'help-echo (concat "Text scale " text-scale-mode-lighter))
     (+modeline--spc))))

(defun +modeline--eglot ()
  "Eglot in mode-line."
  (when (bound-and-true-p eglot--managed-mode)
    (concat
     (+modeline--spc)
     (propertize
      eglot-menu-string
      'face (+modeline--face 'eglot-mode-line)
      'help-echo "Eglot
mouse-1: Eglot Menu
mouse-3: Eglot Server Menu"
      'mouse-face '+modeline-highlight
      'local-map (let ((map (make-sparse-keymap)))
                   (keymap-set map "<mode-line> <mouse-1>" eglot-menu)
                   (keymap-set map "<mode-line> <mouse-3>" eglot-server-menu)
                   map))
     (+modeline--spc))))

(defun +modeline--major-mode ()
  "Major mode in mode-line."
  (concat
   (+modeline--spc)
   (propertize
    (format-mode-line mode-name)
    'face (+modeline--face '+modeline-buffer-major-mode)
    'help-echo "Major mode
mouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes"
    'mouse-face '+modeline-highlight
    'local-map mode-line-major-mode-keymap)
   (+modeline--spc)))

(defun +modeline--vc-info ()
  "Version control info in mode-line."
  (let ((meta (string-trim (format-mode-line '(vc-mode vc-mode)))))
    (unless (string-empty-p meta)
      (concat
       (+modeline--spc)
       (propertize (concat "@" meta) 'face (+modeline--face '+modeline-vc-info))
       (+modeline--spc)))))

(defun +modeline--flymake ()
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
           (.debug 0))
      (maphash (lambda (_b state)
                 (cl-loop
                  with diags = (flymake--state-diags state)
                  for diag in diags do
                  (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                 (warning-numeric-level :error))))
                    (cond ((> severity warning-level) (cl-incf .error))
                          ((> severity debug-level) (cl-incf .warning))
                          (t (cl-incf .debug))))))
               flymake--state)
      (when-let* ((text
                   (cond
                    (some-waiting nil)
                    ((null known) nil)
                    (all-disabled nil)
                    (t (when (> (+ .error .warning .debug) 0)
                         (concat
                          (propertize
                           (number-to-string .error)
                           'face (+modeline--face '+modeline-error))
                          (propertize "/" 'face (+modeline--face))
                          (propertize
                           (number-to-string .warning)
                           'face (+modeline--face '+modeline-warning))
                          (propertize "/" 'face (+modeline--face))
                          (propertize
                           (number-to-string .debug)
                           'face (+modeline--face '+modeline-debug))))))))
        (concat
         (+modeline--spc)
         (propertize
          "!"
          'face
          (if (> .error 0)
              (+modeline--face '+modeline-error)
            (if (> .warning 0)
                (+modeline--face '+modeline-warning)
              (+modeline--face '+modeline-debug))))
         (propertize
          text
          'help-echo
          (format "Flymake
error:%d, warning:%d, debug:%d
mouse-1: Next error
mouse-2: Show all errors
mouse-3: Previous error"
                  .error .warning .debug)
          'mouse-face '+modeline-highlight
          'local-map (let ((map (make-sparse-keymap)))
                       (keymap-set map "<mode-line> <mouse-1>" #'flymake-goto-next-error)
                       (keymap-set map "<mode-line> <mouse-2>" #'flymake-show-buffer-diagnostics)
                       (keymap-set map "<mode-line> <mouse-3>" #'flymake-goto-prev-error)
                       map))
         (+modeline--spc))))))

(defcustom +modeline-left
  '(+modeline--window-number
    +modeline--workspace-name
    +modeline--buffer-info
    +modeline--position
    +modeline--word-count)
  "List of items on the left of mode-line."
  :type '(list function)
  :group '+modeline)

(defcustom +modeline-right
  '(+modeline--text-scale
    +modeline--buffer-encoding
    +modeline--eglot
    +modeline--major-mode
    +modeline--vc-info
    +modeline--flymake)
  "List of items on the right of mode-line."
  :type '(list function)
  :group '+modeline)

(defun +modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(defun +modeline--format (left-segments right-segments)
  "Return a string from LEFT-SEGMENTS and RIGHT-SEGMENTS."
  (let* ((left-str (+modeline--format-segments left-segments))
         (right-str (+modeline--format-segments right-segments))
         (right-width (progn
                        (add-face-text-property 0 (length right-str) 'mode-line t right-str)
                        (string-pixel-width right-str))))
    (concat
     left-str
     (propertize
      " "
      'face (+modeline--face)
      'display
      `(space :align-to (,(- (window-pixel-width)
                             (window-scroll-bar-width)
                             (window-right-divider-width)
                             (* (or (cdr (window-margins)) 1)
                                (frame-char-width))
                             right-width))))
     right-str)))

(defun +modeline--enable ()
  "Enable +modeline."
  (setq-default mode-line-format
                '((:eval (+modeline--format
                          +modeline-left
                          +modeline-right)))))

(defun +modeline--disable ()
  "Disable +modeline."
  (setq-default mode-line-format nil))

(define-minor-mode +modeline-mode
  "Toggle `+modeline' on or off."
  :group '+modeline
  :global t
  :lighter nil
  :keymap nil
  (if +modeline-mode
      (+modeline--enable)
    (+modeline--disable)))

(provide 'init-modeline)
;;; init-modeline.el ends here
