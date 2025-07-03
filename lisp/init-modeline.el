;;; init-modeline.el --- modeline  -*- lexical-binding: t -*-
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

(defface +modeline-default-face
  '((t ()))
  "Default face."
  :group '+modeline-faces)

(defface +modeline-emphasis-face
  '((t (:inherit (+modeline-default-face mode-line-emphasis))))
  "Face used for emphasis."
  :group '+modeline-faces)

(defface +modeline-highlight-face
  '((t (:inherit (+modeline-default-face mode-line-highlight))))
  "Face used for highlighting."
  :group '+modeline-faces)

(defface +modeline-buffer-path-face
  '((t (:inherit (+modeline-emphasis-face bold))))
  "Face used for the dirname part of the buffer path."
  :group '+modeline-faces)

(defface +modeline-buffer-file-face
  '((t (:inherit (+modeline-default-face mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+modeline-faces)

(defface +modeline-buffer-modified-face
  '((t (:inherit (+modeline-default-face warning bold) :background unspecified)))
  "Face used for the \\='unsaved\\=' symbol in the mode-line."
  :group '+modeline-faces)

(defface +modeline-buffer-major-mode-face
  '((t (:inherit (+modeline-emphasis-face bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+modeline-faces)

(defface +modeline-debug-face
  '((t (:inherit (+modeline-default-face success))))
  "Face for debug-level messages in the mode-line."
  :group '+modeline-faces)

(defface +modeline-warning-face
  '((t (:inherit (+modeline-default-face warning))))
  "Face for warnings in the mode-line."
  :group '+modeline-faces)

(defface +modeline-error-face
  '((t (:inherit (+modeline-default-face error))))
  "Face for errors in the mode-line."
  :group '+modeline-faces)

(defvar flymake--state)
(defvar flymake-menu)

(declare-function flymake--diag-type "flymake" t t)
(declare-function flymake--handle-report "flymake")
(declare-function flymake--lookup-type-property "flymake")
(declare-function flymake--state-diags "flymake" t t)
(declare-function flymake-disabled-backends "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function flymake-running-backends "flymake")
(declare-function warning-numeric-level "warnings")

(defun +modeline-face (&optional face)
  "Display FACE in the selected window."
  (if (mode-line-window-selected-p)
      (or (and (facep face) `(:inherit (+modeline-default-face ,face)))
          '(:inherit (+modeline-default-face mode-line-active)))
    (or (and (facep face) `(:inherit (+modeline-default-face mode-line-inactive ,face)))
        '(:inherit (+modeline-default-face mode-line-inactive)))))

(defun +modeline-display-text (text)
  "Display TEXT in mode-line."
  (if (mode-line-window-selected-p)
      text
    (propertize text 'face `(:inherit (mode-line-inactive ,(get-text-property 0 'face text))))))

(defsubst +modeline--spc ()
  "Whitespace."
  (propertize " " 'face (+modeline-face)))

(defsubst +modeline--wspc ()
  "Wide whitespace."
  (propertize "  " 'face (+modeline-face)))

(defun +modeline--buffer-default-directory ()
  "Display `default-directory'."
  (propertize
   (abbreviate-file-name default-directory)
   'face (+modeline-face '+modeline-buffer-path-face)))

(defun +modeline--buffer-info ()
  "Buffer info in mode-line."
  (concat
   (propertize
    (buffer-name)
    'face (if (and (buffer-modified-p) (not buffer-read-only))
              (+modeline-face '+modeline-buffer-modified-face)
            (+modeline-face '+modeline-buffer-file-face))
    'help-echo (format "Buffer name: %s\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                       (or (buffer-file-name) (buffer-name)))
    'mouse-face '+modeline-highlight-face
    'local-map mode-line-buffer-identification-keymap)
   (+modeline--spc)
   (propertize "%I" 'face (+modeline-face))))

(defun +modeline--position ()
  "Position in mode-line."
  (propertize
   (format-mode-line "%l:%c %p%%")
   'face (+modeline-face)
   'help-echo "Buffer position\nmouse-1: Toggle Line and Column Number Display"
   'mouse-face '+modeline-highlight-face
   'local-map mode-line-column-line-number-mode-map))

(defun +modeline--buffer-encoding ()
  "Buffer encoding in mode-line."
  (concat
   (propertize
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF")
      (1 "CRLF")
      (2 "CR")
      (_ ""))
    'face (+modeline-face)
    'mouse-face '+modeline-highlight-face
    'help-echo "End-of-line style\nmouse-1: Cycle change"
    'local-map (let ((map (make-sparse-keymap)))
                 (keymap-set map "<mode-line> <mouse-1>" #'mode-line-change-eol)
                 map))
   (+modeline--spc)
   (let ((sys (coding-system-plist buffer-file-coding-system)))
     (propertize
      (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
             "UTF-8")
            (t (upcase (symbol-name (plist-get sys :name)))))
      'face (+modeline-face)
      'mouse-face '+modeline-highlight-face
      'help-echo 'mode-line-mule-info-help-echo
      'local-map mode-line-coding-system-map))))

(defun +modeline--major-mode ()
  "Major mode in mode-line."
  (propertize
   (format-mode-line mode-name)
   'face (+modeline-face '+modeline-buffer-major-mode-face)
   'help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
   'mouse-face '+modeline-highlight-face
   'local-map mode-line-major-mode-keymap))

(defvar-local +modeline--vc-info nil)
(defun +modeline--update-vc-info (&rest _)
  "Update version control info in mode-line."
  (setq
   +modeline--vc-info
   (when (and vc-mode buffer-file-name)
     (let* ((backend (vc-backend buffer-file-name))
            (state (vc-state buffer-file-name backend))
            (mode (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+"))))
       (propertize
        (cond ((memq state '(edited added))
               (propertize (concat "*" mode) 'face '+modeline-debug-face))
              ((eq state 'needs-merge)
               (propertize (concat "?" mode) 'face '+modeline-debug-face))
              ((eq state 'needs-update)
               (propertize (concat "!" mode) 'face '+modeline-warning-face))
              ((memq state '(removed conflict unregistered))
               (propertize (concat "!" mode) 'face '+modeline-error-face))
              (t
               (propertize (concat "@" mode) 'face '+modeline-debug-face)))
        'help-echo (get-text-property 0 'help-echo mode)
        'mouse-face '+modeline-highlight-face
        'local-map (get-text-property 0 'local-map mode))))))

(defun +modeline--vc-info ()
  "Version control info in mode-line."
  (+modeline-display-text +modeline--vc-info))

(defvar-local +modeline--flymake nil)
(defun +modeline--update-flymake (&rest _)
  "Update flymake in mode-line."
  (setq
   +modeline--flymake
   (let* ((known (hash-table-keys flymake--state))
          (running (flymake-running-backends))
          (disabled (flymake-disabled-backends))
          (reported (flymake-reporting-backends))
          (all-disabled (and disabled (null running)))
          (some-waiting (cl-set-difference running reported)))
     (cond
      (some-waiting (propertize "!!" 'face '+modeline-debug-face))
      ((null known) (propertize "!!" 'face '+modeline-error-face))
      (all-disabled (propertize "!!" 'face '+modeline-warning-face))
      (t (let ((warning-level (warning-numeric-level :warning))
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
           (propertize
            (concat
             (propertize "!" 'face (cond ((> .error 0) '+modeline-error-face)
                                         ((> .warning 0) '+modeline-warning-face)
                                         (t '+modeline-debug-face)))
             (propertize (number-to-string .error) 'face '+modeline-error-face)
             "/"
             (propertize (number-to-string .warning) 'face '+modeline-warning-face)
             "/"
             (propertize (number-to-string .debug) 'face '+modeline-debug-face))
            'help-echo (format "Flymake\nerror:%d, warning:%d, debug:%d\nmouse-1: Flymake menu"
                               .error .warning .debug)
            'mouse-face '+modeline-highlight-face
            'local-map (let ((map (make-sparse-keymap)))
                         (keymap-set map "<mode-line> <mouse-1>" flymake-menu)
                         map))))))))

(defun +modeline--flymake ()
  "Flymake in mode-line."
  (when (and (bound-and-true-p flymake-mode) (bound-and-true-p flymake--state))
    (+modeline-display-text +modeline--flymake)))

(defcustom +modeline-left
  '(+modeline--buffer-info
    +modeline--position)
  "List of items on the left of mode-line."
  :type '(list function)
  :group '+modeline)

(defcustom +modeline-right
  '(+modeline--buffer-encoding
    +modeline--major-mode
    +modeline--vc-info
    +modeline--flymake)
  "List of items on the right of mode-line."
  :type '(list function)
  :group '+modeline)

(defun +modeline--format (segments)
  "Return a string from a list of SEGMENTS."
  (mapconcat
   'identity
   (cl-remove-if
    #'(lambda (n) (eq (length n) 0))
    (mapcar
     #'(lambda (mod) (ignore-errors (funcall mod)))
     segments))
   (+modeline--wspc)))

(defun +modeline--enable ()
  "Enable +modeline."
  (setq-default mode-line-right-align-edge 'right-margin
                mode-line-format
                '((:eval (+modeline--spc))
                  (:eval (+modeline--format +modeline-left))
                  (:eval (+modeline--spc))
                  mode-line-format-right-align
                  (:eval (+modeline--spc))
                  (:eval (+modeline--format +modeline-right))
                  (:eval (+modeline--spc))))
  (add-hook 'find-file-hook #'+modeline--update-vc-info)
  (add-hook 'after-save-hook #'+modeline--update-vc-info)
  (advice-add #'vc-refresh-state :after #'+modeline--update-vc-info)
  (advice-add #'flymake--handle-report :after #'+modeline--update-flymake))

(defun +modeline--disable ()
  "Disable +modeline."
  (setq-default mode-line-format nil)
  (remove-hook 'find-file-hook #'+modeline--update-vc-info)
  (remove-hook 'after-save-hook #'+modeline--update-vc-info)
  (advice-remove #'vc-refresh-state #'+modeline--update-vc-info)
  (advice-remove #'flymake--handle-report #'+modeline--update-flymake))

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
