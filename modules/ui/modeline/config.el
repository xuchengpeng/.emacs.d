;;; ui/modeline/config.el -*- lexical-binding: t; -*-

;; This mode-line is experimental, Emacs 26+ only, may have buggy and is likely
;; to change. It also isn't feature complete, compared to :ui dotemacs-modeline, but
;; it will eventually replace it.
;;
;; However, it is at least ten times faster than the original modeline, and more
;; flexible, what with `+modeline-format-left', `+modeline-format-right', and a
;; more powerful API for defining modelines and modeline segments.

;;;; Benchmarks
;; (benchmark-run 1000 (format-mode-line mode-line-format))
;; Old system: ~0.563 - 0.604
;; New system: ~0.036 - 0.061

(defvar +modeline-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +modeline-height 23
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +modeline-bar-at-end nil
  "If non-nil, the bar is placed at the end, instead of at the beginning of the
modeline.")

(defvar +modeline-bar-invisible nil
  "If non-nil, the bar is transparent, and only used to police the height of the
mode-line.")

(defvar +modeline-buffer-path-function #'+modeline-file-path
  "The function that returns the buffer name display for file-visiting
buffers.")

;; Convenience aliases
(defvaralias 'mode-line-format-left '+modeline-format-left)
(defvaralias 'mode-line-format-right '+modeline-format-right)
;;
(defvar-local +modeline-format-left  () "TODO")
(defvar-local +modeline-format-right () "TODO")
(put '+modeline-format-left  'risky-local-variable t)
(put '+modeline-format-right 'risky-local-variable t)

;;
(defvar +modeline--vspc (propertize " " 'face 'variable-pitch))

;;
;; Custom faces
;;

(defgroup +modeline nil
  "TODO"
  :group 'faces)

(defface dotemacs-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+modeline)

(defface dotemacs-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+modeline)

(defface dotemacs-modeline-buffer-modified '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+modeline)

(defface dotemacs-modeline-buffer-major-mode '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+modeline)

(defface dotemacs-modeline-highlight '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+modeline)

(defface dotemacs-modeline-info `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+modeline)

(defface dotemacs-modeline-warning `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+modeline)

(defface dotemacs-modeline-urgent `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+modeline)

(defface dotemacs-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+modeline)


;;
;; Hacks
;;

;; Keep `+modeline-current-window' up-to-date
(defvar +modeline-current-window (frame-selected-window))

(defun +modeline|set-selected-window (&rest _)
  "Sets `+modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +modeline-current-window win)
      (force-mode-line-update))))

(defun +modeline|unset-selected-window ()
  (setq +modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'+modeline|set-selected-window)
;; (add-hook 'focus-in-hook #'+modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'+modeline|set-selected-window)
(advice-add #'select-window :after #'+modeline|set-selected-window)
(if (not (boundp 'after-focus-change-function))
    (progn
      (add-hook 'focus-in-hook  #'+modeline|set-selected-window)
      (add-hook 'focus-out-hook #'+modeline|unset-selected-window))
  (defun +modeline|refresh-frame ()
    (setq +modeline-current-window nil)
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (setq +modeline-current-window (frame-selected-window frame)))
    (force-mode-line-update t))
  (add-function :after after-focus-change-function #'+modeline|refresh-frame))

(defsubst active ()
  (eq (selected-window) +modeline-current-window))

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar +modeline-remap-face-cookies nil)

(defun +modeline|focus-all-windows (&rest _)
  (cl-loop for (buffer . cookie) in +modeline-remap-face-cookies
           if (buffer-live-p buffer)
           do (with-current-buffer buffer
                (face-remap-remove-relative cookie))))

(defun +modeline|unfocus-all-windows (&rest _)
  (setq +modeline-remap-face-cookies
        (cl-loop for window in (window-list)
                 for buffer = (window-buffer window)
                 if (buffer-live-p buffer)
                 collect
                 (with-current-buffer buffer
                   (cons buffer
                         (face-remap-add-relative 'mode-line
                                                  'mode-line-inactive))))))

(add-hook 'focus-in-hook #'+modeline|focus-all-windows)
(add-hook 'focus-out-hook #'+modeline|unfocus-all-windows)
(advice-add #'posframe-hide :after #'+modeline|focus-all-windows)
(advice-add #'posframe-delete :after #'+modeline|focus-all-windows)
(when (fboundp 'helm-mode)
  (add-hook 'helm-before-initialize-hook #'+modeline|unfocus-all-windows)
  (add-hook 'helm-cleanup-hook #'+modeline|focus-all-windows))


;;
;; Helpers
;;

(defun +modeline--make-xpm (width height &optional color)
  "Create an XPM bitmap. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data)) (length data) color color)
         (cl-loop with idx = 0
                  with len = (length data)
                  for dl in data
                  do (cl-incf idx)
                  concat "\""
                  concat (cl-loop for d in dl
                                  if (= d 0) collect (string-to-char " ")
                                  else collect (string-to-char "."))
                  concat (if (eq idx len) "\"};" "\",\n")))
        'xpm t :ascent 'center)))))

(defun dotemacs-project-root ()
  "Return the root of your project, or `default-directory' if none was found."
  (when (fboundp 'projectile-project-root)
    (let (projectile-require-project-root)
      (projectile-project-root))))

(defun +modeline-file-path (&optional path)
  (let ((buffer-file-name (or path buffer-file-name))
        (root (dotemacs-project-root)))
    (cond ((null root)
           (propertize "%b" 'face 'dotemacs-modeline-buffer-file))
          ((or (null buffer-file-name)
               (directory-name-p buffer-file-name))
           (propertize (abbreviate-file-name (or buffer-file-name default-directory))
                       'face 'dotemacs-modeline-buffer-path))
          ((let* ((true-filename (file-truename buffer-file-name))
                  (relative-dirs (file-relative-name (file-name-directory true-filename)
                                                     (concat root "../"))))
             (if (equal "./" relative-dirs) (setq relative-dirs ""))
             (concat (propertize relative-dirs
                                 'face 'dotemacs-modeline-buffer-path)
                     (propertize (file-name-nondirectory true-filename)
                                 'face 'dotemacs-modeline-buffer-file)))))))


;;
;; Bars
;;

(defvar +modeline-bar-start nil "TODO")
(put '+modeline-bar-start 'risky-local-variable t)
(defvar +modeline-bar-end nil "TODO")
(put '+modeline-bar-end 'risky-local-variable t)

(defvar +modeline-bar-active nil "TODO")
(defvar +modeline-bar-inactive nil "TODO")
(defun +modeline|setup-bars ()
  (setq +modeline-bar-active
        (+modeline--make-xpm +modeline-width +modeline-height
                             (unless +modeline-bar-invisible
                               (face-background 'dotemacs-modeline-bar nil t)))
        +modeline-bar-inactive
        (+modeline--make-xpm +modeline-width +modeline-height))
  (setq +modeline-bar-start nil
        +modeline-bar-end nil)
  (if +modeline-bar-at-end
      (setq +modeline-bar-end '+modeline-bar)
    (setq +modeline-bar-start '+modeline-bar)))
(add-hook 'dotemacs-init-hook #'+modeline|setup-bars)

(defun +modeline|setup-bars-after-change (sym val op _where)
  (when (eq op 'set)
    (set sym val)
    (+modeline|setup-bars)))
(add-variable-watcher '+modeline-width  #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-height #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-bar-at-end #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-bar-invisible #'+modeline|setup-bars-after-change)

(def-modeline-segment! +modeline-bar
  (if (active) +modeline-bar-active +modeline-bar-inactive))


;;
;; Segments
;;

(defun +modeline|update-on-change ()
  (+modeline--set-+modeline-buffer-state)
  (remove-hook 'post-command-hook #'+modeline|update-on-change t))
(defun +modeline|start-update-on-change ()
  (add-hook 'post-command-hook #'+modeline|update-on-change nil t))
(add-hook 'first-change-hook #'+modeline|start-update-on-change)

(advice-add #'undo :after #'+modeline--set-+modeline-buffer-state)
(advice-add #'undo-tree-undo :after #'+modeline--set-+modeline-buffer-state)

(def-modeline-segment! +modeline-buffer-state
  :on-hooks (find-file-hook
             read-only-mode-hook
             after-change-functions
             after-save-hook
             after-revert-hook)
  (let ((icon (cond (buffer-read-only
                     (propertize "RO"
                                 'face 'dotemacs-modeline-warning
                                 'help-echo "Buffer is read-only"))
                    ((buffer-modified-p)
                     (propertize "MOD"
                                 'face 'dotemacs-modeline-buffer-modified
                                 'help-echo "Buffer has been modified"))
                    ((and buffer-file-name (not (file-exists-p buffer-file-name)))
                     (propertize "MOD"
                                 'face 'dotemacs-modeline-urgent
                                 'help-echo "Buffer not exists")))))
    (if icon (concat " [" icon "] "))))

(def-modeline-segment! +modeline-buffer-id
  :on-hooks (find-file-hook after-save-hook after-revert-hook)
  :init " %b"
  :faces t
  (concat (if buffer-file-name " %I ")
          (if buffer-file-name
              (funcall +modeline-buffer-path-function buffer-file-name)
            "%b")))

(def-modeline-segment! +modeline-buffer-directory
  (let ((face (if (active) 'dotemacs-modeline-buffer-path)))
    (concat " "
            (propertize (abbreviate-file-name default-directory)
                        'face face))))

(def-modeline-segment! +modeline-vcs
  :on-set (vc-mode)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active)))
        (cond ((memq state '(edited added))
               (if active (setq face 'dotemacs-modeline-info)))
              ((eq state 'needs-merge)
               (if active (setq face 'dotemacs-modeline-info)))
              ((eq state 'needs-update)
               (if active (setq face 'dotemacs-modeline-warning)))
              ((memq state '(removed conflict unregistered))
               (if active (setq face 'dotemacs-modeline-urgent)))
              (t
               (if active (setq face 'font-lock-doc-face))))
        (concat +modeline--vspc
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face)))))))

(def-modeline-segment! +modeline-encoding
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (buffer-file-coding-system)
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                "UTF-8"
              (upcase (symbol-name (plist-get sys :name)))))
          "  "))

(def-modeline-segment! +modeline-major-mode
  (propertize (format-mode-line mode-name)
              'face (if (active) 'dotemacs-modeline-buffer-major-mode)))

;;
(defsubst dotemacs-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local +modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(defun +modeline|enable-word-count ()
  (setq +modeline-enable-word-count t))
(add-hook 'text-mode-hook #'+modeline|enable-word-count)

(def-modeline-segment! +modeline-selection-info
  (let ((beg (region-beginning))
        (end (region-end)))
    (propertize
     (let ((lines (count-lines beg (min end (point-max)))))
       (concat (cond ((bound-and-true-p rectangle-mark-mode)
                      (let ((cols (abs (- (dotemacs-column end)
                                          (dotemacs-column beg)))))
                        (format "%dx%dB" lines cols)))
                     ((> lines 1)
                      (format "%dC %dL" (- end beg) lines))
                     ((format "%dC" (- end beg))))
               (when +modeline-enable-word-count
                 (format " %dW" (count-words beg end)))))
     'face 'dotemacs-modeline-highlight)))

(defun +modeline|enable-selection-info ()
  (add-to-list '+modeline-format-left '+modeline-selection-info t #'eq))
(defun +modeline|disable-selection-info ()
  (setq +modeline-format-left (delq '+modeline-selection-info +modeline-format-left)))
(add-hook 'activate-mark-hook #'+modeline|enable-selection-info)
(add-hook 'deactivate-mark-hook #'+modeline|disable-selection-info)

;; flycheck

(defun +modeline-flycheck-status (status)
  (pcase status
    (`finished
     (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
            (no-errors (cdr (assq 'error error-counts)))
            (no-warnings (cdr (assq 'warning error-counts)))
            (face (cond (no-errors 'dotemacs-modeline-urgent)
                        (no-warnings 'dotemacs-modeline-warning)
                        (t 'dotemacs-modeline-info))))
       (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                   'face (if (active) face))))
    (`interrupted "-")
    (`suspicious '(propertize "?" 'face (if (active) 'dotemacs-modeline-warning)))
    (`running (propertize "?" 'face (if (active) 'dotemacs-modeline-info)))
    (`errored (propertize "!" 'face (if (active) 'dotemacs-modeline-urgent)))
    (`no-checker (propertize "-" 'face (if (active) 'dotemacs-modeline-warning)))
    ;; (`not-checked nil)
    ))

(defun dotemacs-modeline|update-flycheck-segment (&optional status)
  (setq +modeline-flycheck
        (when-let* ((status-str (+modeline-flycheck-status status)))
          (concat +modeline--vspc status-str " "))))
(add-hook 'flycheck-mode-hook #'dotemacs-modeline|update-flycheck-segment)
(add-hook 'flycheck-status-changed-functions #'dotemacs-modeline|update-flycheck-segment)

(def-modeline-segment! +modeline-flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  :init nil)

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

(def-modeline-segment! +modeline-buffer-position
  "The buffer position information."
  '(" " mode-line-position " "))

;;
;; Preset modeline formats
;;

(def-modeline-format! :main
  '(+modeline-buffer-id
    +modeline-buffer-position)
  `(mode-line-misc-info
    +modeline-encoding
    +modeline-major-mode " "
    (vc-mode ("" +modeline-vcs " "))
    mode-line-process
    +modeline-flycheck))

(def-modeline-format! :minimal
  '(+modeline-buffer-id)
  '(+modeline-major-mode))

(def-modeline-format! :special
  '(" %b " +modeline-buffer-position)
  '(+modeline-encoding +modeline-major-mode mode-line-process))

(def-modeline-format! :project
  '(+modeline-buffer-directory)
  '(+modeline-major-mode))


;;
;;
;;

(def-modeline-segment! +modeline--rest
  (let ((rhs-str (format-mode-line +modeline-format-right)))
    (list (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(1+ (string-width rhs-str))))))
          rhs-str)))

(setq-default mode-line-format '("" +modeline-bar-start +modeline-format-left +modeline--rest +modeline-bar-end))


;;
(set-modeline! :main t)
;; (add-hook! '+doom-dashboard-mode-hook (set-modeline! :project))
;; (add-hook! 'doom-scratch-buffer-hook  (set-modeline! :special))
