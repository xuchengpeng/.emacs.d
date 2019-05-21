;;; dotemacs-modeline-segments.el --- The segments for dotemacs-modeline -*- lexical-binding: t; -*-

;;; Code:

;;
;; Externals
;;

(defvar anzu--cached-count)
(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--total-matched)
(defvar anzu-cons-mode-line-p)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-mc-frozen)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar flycheck-current-errors)
(defvar flycheck-mode-menu-map)
(defvar flymake--backend-state)
(defvar flymake--mode-line-format)
(defvar flymake-menu)
(defvar iedit-occurrences-overlays)
(defvar mc/mode-line)
(defvar symbol-overlay-keywords-alist)
(defvar symbol-overlay-temp-symbol)
(defvar text-scale-mode-amount)

(declare-function anzu--reset-status 'anzu)
(declare-function anzu--where-is-here 'anzu)
(declare-function evil-delimited-arguments 'evil-common)
(declare-function evil-emacs-state-p 'evil-states)
(declare-function evil-force-normal-state 'evil-commands)
(declare-function evil-insert-state-p 'evil-states)
(declare-function evil-motion-state-p 'evil-states)
(declare-function evil-normal-state-p 'evil-states)
(declare-function evil-operator-state-p 'evil-states)
(declare-function evil-replace-state-p 'evil-states)
(declare-function evil-state-property 'evil-common)
(declare-function evil-visual-state-p 'evil-states)
(declare-function flycheck-buffer 'flycheck)
(declare-function flycheck-count-errors 'flycheck)
(declare-function flycheck-list-errors 'flycheck)
(declare-function flycheck-next-error 'flycheck)
(declare-function flycheck-previous-error 'flycheck)
(declare-function flymake--backend-state-diags 'flymake)
(declare-function flymake--diag-type 'flymake)
(declare-function flymake--handle-report 'flymake)
(declare-function flymake--lookup-type-property 'flymake)
(declare-function flymake-disabled-backends 'flymake)
(declare-function flymake-goto-next-error 'flymake)
(declare-function flymake-goto-prev-error 'flymake)
(declare-function flymake-reporting-backends 'flymake)
(declare-function flymake-running-backends 'flymake)
(declare-function flymake-show-diagnostics-buffer 'flymake)
(declare-function flymake-start 'flymake)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function magit-toplevel 'magit-git)
(declare-function mc/num-cursors 'multiple-cursors-core)
(declare-function symbol-overlay-assoc 'symbol-overlay)
(declare-function symbol-overlay-get-list 'symbol-overlay)
(declare-function symbol-overlay-get-symbol 'symbol-overlay)
(declare-function symbol-overlay-rename 'symbol-overlay)
(declare-function tracking-shorten 'tracking)
(declare-function undo-tree-redo-1 'undo-tree)
(declare-function undo-tree-undo-1 'undo-tree)
(declare-function warning-numeric-level 'warnings)

;;
;; buffer information
;;

(dotemacs-modeline-def-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((active (dotemacs-modeline--active)))
    (concat (dotemacs-modeline-whitespace)
            (propertize (abbreviate-file-name default-directory)
                        'face (if active
                                  'dotemacs-modeline-buffer-path
                                'mode-line-inactive)))))

(dotemacs-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   " %b "
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'dotemacs-modeline-buffer-modified)
               ((dotemacs-modeline--active) 'dotemacs-modeline-buffer-file)
               (t 'mode-line-inactive))))

;;
;; encoding
;;

(dotemacs-modeline-def-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (propertize
   (concat (pcase (coding-system-eol-type buffer-file-coding-system)
             (0 " LF")
             (1 " CRLF")
             (2 " CR"))
           (let ((sys (coding-system-plist buffer-file-coding-system)))
             (cond ((memq (plist-get sys :category)
                          '(coding-category-undecided coding-category-utf-8))
                    " UTF-8 ")
                   (t (upcase (symbol-name (plist-get sys :name)))))))
   'face (if (dotemacs-modeline--active) 'mode-line 'mode-line-inactive)
   'help-echo 'mode-line-mule-info-help-echo
   'mouse-face '(:box 0)
   'local-map mode-line-coding-system-map))

;;
;; indentation
;;

(dotemacs-modeline-def-segment indent-info
  "Displays the indentation information."
  (propertize (format " %s %d "
                      (if indent-tabs-mode "TAB" "SPC") tab-width)
              'face (if (dotemacs-modeline--active) 'mode-line 'mode-line-inactive)))

;;
;; remote host
;;

(dotemacs-modeline-def-segment remote-host
  "Hostname for remote buffers."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize
       (concat "@" host)
       'face (if (dotemacs-modeline--active) 'mode-line 'mode-line-inactive)))))

;;
;; major-mode
;;

(dotemacs-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (dotemacs-modeline-whitespace)
    (propertize mode-name
                'help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                'mouse-face 'mode-line-highlight
                'local-map mode-line-major-mode-keymap)
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (dotemacs-modeline-whitespace))
   'face (if (dotemacs-modeline--active)
             'dotemacs-modeline-buffer-major-mode
           'mode-line-inactive)))

;;
;; minor modes
;;

(dotemacs-modeline-def-segment minor-modes
  (when dotemacs-modeline-minor-modes
    (let ((active (dotemacs-modeline--active)))
      (propertize
       (concat
        (replace-regexp-in-string (regexp-quote "%")
                                  "%%%%"
                                  (format-mode-line '("" minor-mode-alist))
                                  t t)
        " ")
       'face (if active
                 'dotemacs-modeline-buffer-minor-mode
               'mode-line-inactive)))))

;;
;; process
;;

(dotemacs-modeline-def-segment process
  "The process info."
  (if (dotemacs-modeline--active)
      mode-line-process
    (propertize
     (format-mode-line '("" mode-line-process))
     'face 'mode-line-inactive)))

(defvar dotemacs-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control.")
(defvar-local dotemacs-modeline--vcs-text nil)
(defun dotemacs-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (setq dotemacs-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (str (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
            (propertize (if (> (length str) dotemacs-modeline-vcs-max-length)
                            (concat
                             (substring str 0 (- dotemacs-modeline-vcs-max-length 3))
                             "...")
                          str)
                        'face (cond ((eq state 'needs-update)
                                     'dotemacs-modeline-warning)
                                    ((memq state '(removed conflict unregistered))
                                     'dotemacs-modeline-urgent)
                                    (t 'dotemacs-modeline-info)))))))
(add-hook 'find-file-hook #'dotemacs-modeline-update-vcs-text t)
(add-hook 'after-save-hook #'dotemacs-modeline-update-vcs-text)
(advice-add #'vc-refresh-state :after #'dotemacs-modeline-update-vcs-text)

(dotemacs-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (let ((active (dotemacs-modeline--active)))
    (when-let ((text (or dotemacs-modeline--vcs-text (dotemacs-modeline-update-vcs-text))))
      (concat
       (dotemacs-modeline-whitespace)
       (if active
           text
         (propertize text 'face 'mode-line-inactive))
       (dotemacs-modeline-whitespace)))))

(defun dotemacs-modeline-checker-text (text &optional face)
  "Displays TEXT with FACE."
  (when text
    (propertize text 'face face)))

(defvar-local dotemacs-modeline--flycheck-text nil)
(defun dotemacs-modeline-update-flycheck-text (&optional status)
  "Update flycheck text via STATUS."
  (setq dotemacs-modeline--flycheck-text
        (when-let
            ((text
              (pcase status
                (`finished  (when flycheck-current-errors
                              (let-alist (flycheck-count-errors flycheck-current-errors)
                                (let ((error (or .error 0))
                                      (warning (or .warning 0))
                                      (info (or .info 0)))
                                  (format "%s/%s/%s"
                                          (dotemacs-modeline-checker-text (number-to-string error)
                                                                      'dotemacs-modeline-urgent)
                                          (dotemacs-modeline-checker-text (number-to-string warning)
                                                                      'dotemacs-modeline-warning)
                                          (dotemacs-modeline-checker-text (number-to-string info)
                                                                        'dotemacs-modeline-info))))))
                (`running     nil)
                (`no-checker  (dotemacs-modeline-checker-text "-" 'dotemacs-modeline-debug))
                (`errored     (dotemacs-modeline-checker-text "Error" 'dotemacs-modeline-urgent))
                (`interrupted (dotemacs-modeline-checker-text "Interrupted" 'dotemacs-modeline-debug))
                (`suspicious  (dotemacs-modeline-checker-text "Suspicious" 'dotemacs-modeline-urgent))
                (_ nil))))
          (propertize
           text
           'help-echo (pcase status
                        ('finished
                         (concat
                          (if flycheck-current-errors
                              (let-alist (flycheck-count-errors flycheck-current-errors)
                                (format "error: %d, warning: %d, info: %d\n"
                                        (or .error 0) (or .warning 0) (or .info 0))))
                          "mouse-1: Show all errors
mouse-3: Next error"
                          (if (featurep 'mwheel)
                              "\nwheel-up/wheel-down: Previous/next error")))
                        ('running "Running...")
                        ('no-checker "No Checker")
                        ('errored "Error")
                        ('interrupted "Interrupted")
                        ('suspicious "Suspicious"))
           'mouse-face 'mode-line-highlight
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line mouse-1]
                          #'flycheck-list-errors)
                        (define-key map [mode-line mouse-3]
                          #'flycheck-next-error)
                        (when (featurep 'mwheel)
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-down-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-previous-error 1))))
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-up-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-next-error 1))))
                          map))))))
(add-hook 'flycheck-status-changed-functions #'dotemacs-modeline-update-flycheck-text)
(add-hook 'flycheck-mode-hook #'dotemacs-modeline-update-flycheck-text)

(defvar-local dotemacs-modeline--flymake-text nil)
(defun dotemacs-modeline-update-flymake-text (&rest _)
  "Update flymake text."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq dotemacs-modeline--flymake-text
        (let* ((known (hash-table-keys flymake--backend-state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported))
               (warning-level (warning-numeric-level :warning))
               (note-level (warning-numeric-level :debug))
               (.error 0)
               (.warning 0)
               (.note 0))
          (maphash (lambda (_b state)
                     (cl-loop
                      with diags = (flymake--backend-state-diags state)
                      for diag in diags do
                      (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                     (warning-numeric-level :error))))
                        (cond ((> severity warning-level) (cl-incf .error))
                              ((> severity note-level) (cl-incf .warning))
                              (t (cl-incf .note))))))
                   flymake--backend-state)
          (when-let
              ((text
                (cond
                 (some-waiting "Running..." "")
                 ((null known) (dotemacs-modeline-checker-text "-" 'dotemacs-modeline-debug))
                 (all-disabled (dotemacs-modeline-checker-text "-" 'dotemacs-modeline-urgent))
                 (t (let ((num (+ .error .warning .note)))
                      (when (> num 0)
                        (format "%s/%s/%s"
                                (dotemacs-modeline-checker-text (number-to-string .error)
                                                            'dotemacs-modeline-urgent)
                                (dotemacs-modeline-checker-text (number-to-string .warning)
                                                            'dotemacs-modeline-warning)
                                (dotemacs-modeline-checker-text (number-to-string .note)
                                                            'dotemacs-modeline-info))))))))
            (propertize
             text
             'help-echo (cond
                         (some-waiting "Running...")
                         ((null known) "No Checker")
                         (all-disabled "All Checkers Disabled")
                         (t (format "error: %d, warning: %d, note: %d
mouse-1: List all problems%s"
                                    .error .warning .note
                                    (if (featurep 'mwheel)
                                        "\nwheel-up/wheel-down: Previous/next problem"))))
             'mouse-face 'mode-line-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1]
                            #'flymake-show-diagnostics-buffer)
                          (when (featurep 'mwheel)
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-down-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-prev-error 1 nil t))))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-up-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-next-error 1 nil t))))
                            map)))))))
(advice-add #'flymake--handle-report :after #'dotemacs-modeline-update-flymake-text)

(dotemacs-modeline-def-segment checker
  "Displays color-coded error status in the current buffer with pretty
icons."
  (let ((active (dotemacs-modeline--active))
        (text (cond ((bound-and-true-p flycheck-mode)
                     dotemacs-modeline--flycheck-text)
                    ((and (bound-and-true-p flymake-mode)
                         (bound-and-true-p flymake--backend-state)) ; only support 26+
                     dotemacs-modeline--flymake-text))))
    (when text
      (concat
       (dotemacs-modeline-whitespace)
       (if active
           text
         (propertize text 'face 'mode-line-inactive))
       (dotemacs-modeline-whitespace)))))

;;
;; selection-info
;;

(defsubst dotemacs-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(dotemacs-modeline-def-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (dotemacs-modeline--active))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat " "
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
                       ((format "%dC" (- end beg))))
                 (when dotemacs-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))
                 " "))
       'face 'dotemacs-modeline-highlight))))

;;
;; matches (macro, anzu, evil-substitute, iedit, symbol-overlay and multi-cursors)
;;

(defsubst dotemacs-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (dotemacs-modeline--active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'dotemacs-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'dotemacs-modeline-panel)
              sep
              (propertize ">" 'face 'dotemacs-modeline-panel)
              sep))))

(defsubst dotemacs-modeline--anzu ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " anzu--cached-count))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel 'mode-line-inactive))))

(defsubst dotemacs-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-local-mode)
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
     'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel 'mode-line-inactive))))

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
                        (progn (iedit-prev-occurrence)
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
     'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel 'mode-line-inactive))))

(defsubst dotemacs-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when (and (dotemacs-modeline--active)
             (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol))
             (not (bound-and-true-p iedit-mode)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol nil t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (propertize
           (format (concat  " %d/%d " (and (cadr keyword) "in scope "))
                   (+ count 1)
                   (+ count (length after)))
           'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel 'mode-line-inactive))))))

(defsubst dotemacs-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (cl-destructuring-bind (count . face)
      (cond ((bound-and-true-p multiple-cursors-mode)
             (cons (mc/num-cursors)
                   (if (dotemacs-modeline--active)
                       'dotemacs-modeline-panel
                     'mode-line-inactive)))
            ((bound-and-true-p evil-mc-cursor-list)
             (cons (length evil-mc-cursor-list)
                   (cond ((not (dotemacs-modeline--active)) 'mode-line-inactive)
                         (evil-mc-frozen 'dotemacs-modeline-bar)
                         ('dotemacs-modeline-panel))))
            ((cons nil nil)))
    (when count
      (propertize (format " %d " count)
                  'face face))))

(defsubst dotemacs-modeline--buffer-size ()
  "Show buffer size."
  (if (and buffer-file-name size-indication-mode)
      (propertize " %I "
                  'face (if (dotemacs-modeline--active) 'mode-line 'mode-line-inactive)
                  'help-echo "Buffer size
mouse-1: Display Line and Column Mode Menu"
                  'mouse-face '(:box 0)
                  'local-map mode-line-column-line-number-mode-map)))

(dotemacs-modeline-def-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with `anzu'), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions,
5. The current/total for the highlight term (with `symbol-overlay'), 6. The number
of active `multiple-cursors'."
  (let ((meta (concat (dotemacs-modeline--macro-recording)
                      (dotemacs-modeline--anzu)
                      (dotemacs-modeline--evil-substitute)
                      (dotemacs-modeline--iedit)
                      (dotemacs-modeline--symbol-overlay)
                      (dotemacs-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (dotemacs-modeline--buffer-size))))

(dotemacs-modeline-def-segment buffer-size
  "Display buffer size"
  (dotemacs-modeline--buffer-size))

;;
;; media-info
;;

(dotemacs-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (when (fboundp 'image-size)
               (image-size (image-get-display-property) :pixels))
           (propertize
            (format "  %dx%d  " width height)
            'face (if (dotemacs-modeline--active) 'mode-line 'mode-line-inactive))))))

;;
;; bar
;;

(defvar dotemacs-modeline--bar-active nil)
(defvar dotemacs-modeline--bar-inactive nil)
(dotemacs-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (if (dotemacs-modeline--active)
          dotemacs-modeline--bar-active
        dotemacs-modeline--bar-inactive)
    ""))

(defun dotemacs-modeline-refresh-bars (&optional width height)
  "Refresh mode-line bars with `WIDTH' and `HEIGHT'."
  (setq dotemacs-modeline--bar-active
        (dotemacs-modeline--make-xpm 'dotemacs-modeline-bar
                                 (or width dotemacs-modeline-bar-width)
                                 (max (or height dotemacs-modeline-height)
                                      (ceiling (* 1.3 (frame-char-height)))))
        dotemacs-modeline--bar-inactive
        (dotemacs-modeline--make-xpm 'dotemacs-modeline-inactive-bar
                                 (or width dotemacs-modeline-bar-width)
                                 (max (or height dotemacs-modeline-height)
                                      (ceiling (* 1.3 (frame-char-height)))))))
(add-hook 'dotemacs-init-ui-hook #'dotemacs-modeline-refresh-bars)

;;
;; misc info
;;

(dotemacs-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (if (dotemacs-modeline--active)
      '("" mode-line-misc-info)))

(dotemacs-modeline-def-segment buffer-position
  "The buffer position information."
  '(" " "%2l:%c" " " (-3 "%p") " "))

(provide 'dotemacs-modeline-segments)

;;; dotemacs-modeline-segments.el ends here
