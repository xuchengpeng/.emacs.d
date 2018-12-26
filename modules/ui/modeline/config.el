;;; ui/modeline/config.el -*- lexical-binding: t; -*-

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

(defvar +modeline-buffer-path-function #'+modeline-file-path-with-project
  "A function that returns, in list form, components of the buffer file name
display in the mode-line.

Each item should either be a string or a a cons cell whose CAR is the path
component and CDR is the name of a face.

Currently available functions:

+ `+modeline-file-path-with-project': project/src/lib/file.c
+ `+modeline-file-path-from-project': src/lib/file.c
+ `+modeline-file-path-truncated-with-project': project/s/l/file.c
+ `+modeline-file-path-truncated-upto-project': ~/w/project/src/lib/file.c
+ `+modeline-file-path-truncated-upto-project-root': ~/w/p/s/lib/file.c
+ `+modeline-file-path-truncated': ~/w/p/s/l/file.c
+ `+modeline-file-name': file.c")

;; Convenience aliases
(defvaralias 'mode-line-format-left '+modeline-format-left)
(defvaralias 'mode-line-format-right '+modeline-format-right)
;;
(defvar-local +modeline-format-left  () "TODO")
(defvar-local +modeline-format-right () "TODO")
(put '+modeline-format-left  'risky-local-variable t)
(put '+modeline-format-right 'risky-local-variable t)

;; Otherwise appended segments will produce *Invalid*
(setq global-mode-string '(""))
;; We handle this ourselves
(setq projectile-dynamic-mode-line nil)

;;
(defvar +modeline--vspc (propertize " " 'face 'variable-pitch))

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar evil-visual-beginning nil)
(defvar evil-visual-end nil)
(defvar iedit-mode nil)

;;
;; Custom faces

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

(defface dotemacs-modeline-buffer-project-root
  '((t (:inherit dotemacs-modeline-buffer-path)))
  "Face used for the project root at the beginning of the mode-line path."
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

(defface dotemacs-modeline-panel '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+modeline--anzu',
`+modeline--evil-substitute' and `iedit'"
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
;; Evil state faces

(defface dotemacs-modeline-evil-emacs-state '((t (:inherit dotemacs-modeline-warning)))
  "Face for the Emacs state tag in evil state indicator."
  :group '+modeline)

(defface dotemacs-modeline-evil-insert-state '((t (:inherit dotemacs-modeline-urgent)))
  "Face for the insert state tag in evil state indicator."
  :group '+modeline)

(defface dotemacs-modeline-evil-motion-state '((t :inherit dotemacs-modeline-buffer-path))
  "Face for the motion state tag in evil state indicator."
  :group '+modeline)

(defface dotemacs-modeline-evil-normal-state '((t (:inherit dotemacs-modeline-info)))
  "Face for the normal state tag in evil state indicator."
  :group '+modeline)

(defface dotemacs-modeline-evil-operator-state '((t (:inherit dotemacs-modeline-buffer-path)))
  "Face for the operator state tag in evil state indicator."
  :group '+modeline)

(defface dotemacs-modeline-evil-visual-state '((t (:inherit dotemacs-modeline-buffer-file)))
  "Face for the visual state tag in evil state indicator."
  :group '+modeline)

(defface dotemacs-modeline-evil-replace-state '((t (:inherit dotemacs-modeline-buffer-modified)))
  "Face for the replace state tag in evil state indicator."
  :group '+modeline)

;;
;; Hacks

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
(when (featurep! :completion helm)
  (add-hook 'helm-before-initialize-hook #'+modeline|unfocus-all-windows)
  (add-hook 'helm-cleanup-hook #'+modeline|focus-all-windows))


;;
;; Helpers

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

(defun +modeline-build-path (path)
  "Construct the file path for the `+modeline-buffer-id' segment using
`+mdoeline-buffer-path-function'. If the buffer has no `buffer-file-name', just
use `buffer-name'."
  (let ((buffer-file-name (or path buffer-file-name)))
    (if (or (eq major-mode 'dired-mode)
            (null buffer-file-name))
        (propertize "%s" 'face 'dotemacs-modeline-buffer-path)
      (cl-loop for spec in (funcall +modeline-buffer-path-function)
               if (stringp spec) concat spec
               else if (not (null spec))
               concat (propertize (car spec) 'face (cdr spec))))))


;;
;; Buffer file path styles

(defun +modeline-file-path-with-project ()
  "Returns the unaltered buffer file path relative to the project root's
parent.

e.g. project/src/lib/file.c"
  (let* ((base (buffer-base-buffer))
         (filename (file-truename (buffer-file-name base))))
    (append (if (dotemacs-project-p)
                (let* ((project-root (dotemacs-project-root))
                       (relative-dirs (file-relative-name (file-name-directory filename)
                                                          (file-truename project-root))))
                  (list (cons (concat (dotemacs-project-name) "/")
                              'dotemacs-modeline-buffer-project-root)
                        (unless (equal "./" relative-dirs)
                          (cons relative-dirs 'dotemacs-modeline-buffer-path))))
              (list nil (cons (abbreviate-file-name (file-name-directory filename))
                              'dotemacs-modeline-buffer-path)))
            (list (cons (file-name-nondirectory filename)
                        'dotemacs-modeline-buffer-file)))))

(defun +modeline-file-path-from-project ()
  "Returns file path relative to the project root.

e.g. src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (cdr (+modeline-file-path-with-project)))

(defun +modeline-file-path-truncated-with-project ()
  "Returns file path relative to (and including) project root, with descendent
folders truncated.

e.g. project/s/l/file.c

Meant for `+modeline-buffer-path-function'."
  (let* ((parts (+modeline-file-path-with-project))
         (dirs (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (shrink-path--dirs-internal dirs t))
    parts))

(defun +modeline-file-path-truncated-upto-project ()
  "Returns file path, truncating segments prior to the project.

e.g. ~/w/project/src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (pcase-let
      ((`(,root-parent ,root ,dir, file)
        (let ((buffer-file-name (or buffer-file-name (buffer-file-name (buffer-base-buffer)))))
          (shrink-path-file-mixed (or (dotemacs-project-root) default-directory)
                                  (file-name-directory buffer-file-name)
                                  buffer-file-name))))
    (list (cons root-parent 'font-lock-comment-face)
          (cons root 'dotemacs-modeline-buffer-project-root)
          (cons (concat "/" dir) 'dotemacs-modeline-buffer-path)
          (cons file 'dotemacs-modeline-buffer-file))))

(defun +modeline-file-path-truncated-upto-project-root ()
  "Return file path, truncating segemnts prior to (and including) the project
root.

e.g. ~/w/p/src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (let* ((parts (+modeline-file-path-truncated-upto-project))
         (root (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (let ((first (substring root 0 1)))
              (if (equal first ".")
                  (substring root 0 2)
                first)))
    parts))

(defun +modeline-file-path-truncated ()
  "Return absolute file path with all directories truncated.

e.g. ~/w/p/s/l/file.c

Meant for `+modeline-buffer-path-function'."
  (pcase-let ((`(,dir . ,file)
               (shrink-path-prompt (buffer-file-name (buffer-base-buffer)))))
    (list (cons dir 'dotemacs-modeline-buffer-path)
          (cons file 'dotemacs-modeline-buffer-file))))

(defun +modeline-file-name ()
  "Return buffer name.

e.g. file.c

Meant for `+modeline-buffer-path-function'."
  (list (cons "%b" 'dotemacs-modeline-buffer-path)))

;;
;; Bars

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
(add-hook 'dotemacs-load-theme-hook #'+modeline|setup-bars)

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

(def-modeline-segment! +modeline-buffer-state
  (let* ((active (active))
         (base (buffer-base-buffer))
         (icon (cond (buffer-read-only
                      (propertize "<R>"
                                  'face (if active 'dotemacs-modeline-warning)
                                  'help-echo "Buffer is read-only"))
                     ((buffer-modified-p base)
                      (propertize "<M>"
                                  'face (if active 'dotemacs-modeline-buffer-modified)
                                  'help-echo "Buffer has been modified"))
                     ((and (buffer-file-name base)
                           (not (file-exists-p (buffer-file-name base))))
                      (propertize "<E>"
                                  'face (if active 'dotemacs-modeline-urgent)
                                  'help-echo "Buffer does not exists"))
                     (t
                      (propertize "<N>"
                                  'face (if active 'dotemacs-modeline-info)
                                  'help-echo "Buffer is in normal state")))))
    (if icon (concat " " icon))))

(def-modeline-segment! +modeline-evil-state
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (let ((tag (evil-state-property evil-state :tag t)))
      (propertize tag 'face
                  (if (active)
                      (cond ((evil-normal-state-p) 'dotemacs-modeline-evil-normal-state)
                            ((evil-emacs-state-p) 'dotemacs-modeline-evil-emacs-state)
                            ((evil-insert-state-p) 'dotemacs-modeline-evil-insert-state)
                            ((evil-motion-state-p) 'dotemacs-modeline-evil-motion-state)
                            ((evil-visual-state-p) 'dotemacs-modeline-evil-visual-state)
                            ((evil-operator-state-p) 'dotemacs-modeline-evil-operator-state)
                            ((evil-replace-state-p) 'dotemacs-modeline-evil-replace-state)))))))

(def-modeline-segment! +modeline-buffer-id
  :on-hooks (find-file-hook after-save-hook after-revert-hook)
  :init (propertize " %b" 'face 'dotemacs-modeline-buffer-file)
  :faces t
  (concat " "
          (let ((file-path (buffer-file-name (buffer-base-buffer))))
            (propertize (+modeline-build-path file-path)
                        'help-echo file-path))))

(def-modeline-segment! +modeline-buffer-directory
  (let ((face (if (active) 'dotemacs-modeline-buffer-path)))
    (concat " "
            (propertize (abbreviate-file-name default-directory)
                        'face face
                        'help-echo default-directory))))

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

(def-modeline-segment! +modeline-indent-style
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (indent-tabs-mode tab-width)
  (propertize (format "%s%d  "
                      (if indent-tabs-mode "⭾" "␣")
                      tab-width)
              'help-echo
              (format "Indentation: %d %s wide"
                      tab-width
                      (if indent-tabs-mode "tabs" "spaces"))))

(def-modeline-segment! +modeline-encoding
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (buffer-file-coding-system)
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 (propertize "LF" 'help-echo "EOL convention: \\n (Unix)"))
            (1 (propertize "CRLF" 'help-echo "EOL convention: \\r\\n (Windows, Symbian OS, etc)"))
            (2 (propertize "CR" 'help-echo "EOL convention: \\r (pre-OSX MacOS)")))
          "  "
          (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (category (plist-get sys :category)))
            (propertize
             (cond ((eq category 'coding-category-undecided)
                    "")
                   ((or (eq category 'coding-category-utf-8)
                        (eq (plist-get sys :name) 'prefer-utf-8))
                    "UTF-8  ")
                   ((concat (upcase (symbol-name (plist-get sys :name)))
                            "  ")))
             'help-echo (plist-get (coding-system-plist buffer-file-coding-system) :docstring)))))

(def-modeline-segment! +modeline-major-mode
  (propertize (format-mode-line mode-name)
              'face (if (active) 'dotemacs-modeline-buffer-major-mode)))

(defun +modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'dotemacs-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'dotemacs-modeline-panel)
              sep))))

(defsubst +modeline--anzu ()
  "Show the match index and total number thereof. Requires `anzu', also
`evil-anzu' if using `evil-mode' for compatibility with `evil-search'."
  (when (and anzu--state (not iedit-mode))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             ((format " %s/%d " here total))))
     'face (if (active) 'dotemacs-modeline-panel))))

(defsubst +modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (active) 'dotemacs-modeline-panel))))

(defun dotemacs-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'dotemacs-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) 'dotemacs-modeline-panel))))

(def-modeline-segment! +modeline-matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+modeline--macro-recording)
                      (+modeline--anzu)
                      (+modeline--evil-substitute)
                      (+modeline--iedit)
                      " ")))
     (or (and (not (equal meta " ")) meta)
         (if buffer-file-name " %I"))))

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
  (let ((beg (or evil-visual-beginning (region-beginning)))
        (end (or evil-visual-end (region-end))))
    (propertize
     (let ((lines (count-lines beg (min end (point-max)))))
       (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                          (eq 'block evil-visual-selection))
                      (let ((cols (abs (- (dotemacs-column end)
                                          (dotemacs-column beg)))))
                        (format "%dx%dB" lines cols)))
                     ((eq evil-visual-selection 'line)
                      (format "%dL" lines))
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
(cond ((featurep! :feature evil)
       (add-hook 'evil-visual-state-entry-hook #'+modeline|enable-selection-info)
       (add-hook 'evil-visual-state-exit-hook #'+modeline|disable-selection-info))
      ((add-hook 'activate-mark-hook #'+modeline|enable-selection-info)
       (add-hook 'deactivate-mark-hook #'+modeline|disable-selection-info)))

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
  "Displays color-coded flycheck error status in the current buffer."
  :init nil)

;; position

(def-modeline-segment! +modeline-buffer-position
  "The buffer position information."
  '("  " "%2l:%c" " " (-3 "%p") "  "))

;; misc-info

(def-modeline-segment! +modeline-misc-info
  (propertize (format-time-string " %H:%M  ")
              'help-echo (format-time-string "%Y-%m-%d %H:%M")))

;;
;; Preset modeline formats

(def-modeline-format! :main
  '(+modeline-matches
    +modeline-buffer-state
    +modeline-buffer-id
    +modeline-buffer-position)
  '(+modeline-encoding
    +modeline-major-mode " "
    (vc-mode ("" +modeline-vcs " "))
    +modeline-flycheck))

(def-modeline-format! :minimal
  '(+modeline-matches
    +modeline-buffer-state
    +modeline-buffer-id)
  '(+modeline-major-mode))

(def-modeline-format! :special
  '(+modeline-matches +modeline-buffer-state " %b " +modeline-buffer-position)
  '(+modeline-encoding +modeline-major-mode mode-line-process))

(def-modeline-format! :project
  '(+modeline-buffer-directory)
  '(+modeline-major-mode))


;;
;; Set modeline format

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
(add-hook! '+dashboard-mode-hook (set-modeline! :project))
