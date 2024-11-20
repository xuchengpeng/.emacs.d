;;; init-echo-bar.el --- Display text at end of the echo area. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'timer)

(defgroup echo-bar nil
  "Display text at end of the echo area."
  :group 'applications)

(defcustom echo-bar-modules
  '(echo-bar--selection-info
    echo-bar--symbol-overlay
    echo-bar--multiple-cursors
    echo-bar--time)
  "List of items displayed in the echo bar."
  :group 'echo-bar
  :type '(list function))

(defcustom echo-bar-right-padding 0
  "Number of columns between the text and right margin."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-minibuffer t
  "If non-nil, also display the echo bar when in the minibuffer."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-update-interval 1
  "Interval in seconds between updating the echo bar contents.

If nil, don't update the echo bar automatically."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-separator " "
  "Default string for the separator between modules."
  :group 'echo-bar
  :type 'string)

(declare-function mc/num-cursors "ext:multiple-cursors-core")
(declare-function symbol-overlay-assoc "ext:symbol-overlay")
(declare-function symbol-overlay-get-list "ext:symbol-overlay")
(declare-function symbol-overlay-get-symbol "ext:symbol-overlay")

(defun echo-bar--selection-info ()
  "Display selection info."
  (when mark-active
    (let* ((beg (region-beginning))
           (end (region-end))
           (lines (count-lines beg (min end (point-max)))))
      (concat
       (cond ((bound-and-true-p rectangle-mark-mode)
              (let ((cols (abs (- (save-excursion (goto-char end) (current-column))
                                  (save-excursion (goto-char beg) (current-column))))))
                (format "%dx%dB" lines cols)))
             ((> lines 1)
              (format "%dC %dL" (- end beg) lines))
             (t
              (format "%dC" (- end beg))))
       (format " %dW" (count-words beg end))))))

(defun echo-bar--symbol-overlay ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (when (symbol-overlay-assoc symbol)
        (format "%s:%d/%d" symbol (+ count 1) (+ count (length after)))))))

(defun echo-bar--multiple-cursors ()
  "Display the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (when-let* ((count (mc/num-cursors)))
      (format "mc:%d" count))))

(defun echo-bar--time ()
  "Display time."
  (format-time-string "[%Y/%m/%d %H:%M %a]"))

(defvar echo-bar-text nil
  "The text currently displayed in the echo bar.")

(defvar echo-bar-overlays nil
  "List of overlays displaying the echo bar contents.")

(defun echo-bar--minibuffer-setup ()
  "Setup the echo bar in the minibuffer."
  (when echo-bar-minibuffer
    (push (make-overlay (point-max) (point-max) nil t t) echo-bar-overlays)
    (overlay-put (car echo-bar-overlays) 'priority 1)
    (echo-bar-update)))

(defun echo-bar--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun echo-bar-set-text (text)
  "Set the text displayed by the echo bar to TEXT."
  (unless (active-minibuffer-window)
    (let* ((wid (+ (echo-bar--str-len text) echo-bar-right-padding
                   (if (and (display-graphic-p)
                            (> (nth 1 (window-fringes)) 0)
                            (not overflow-newline-into-fringe)
                            (<= echo-bar-right-padding 0))
                       1
                     0)))
           ;; Align the text to the correct width to make it right aligned
           (spc (propertize " " 'cursor 1 'display
                            `(space :align-to (- right-fringe ,wid)))))

      (setq echo-bar-text (concat spc text))

      ;; Remove all dead overlays from the list
      (setq echo-bar-overlays (seq-filter 'overlay-buffer echo-bar-overlays))

      ;; Add the correct text to each echo-bar overlay
      (dolist (o echo-bar-overlays)
        (when (overlay-buffer o)
          (with-current-buffer (overlay-buffer o)
            ;; Wrap the text to the next line if the echo bar text is too long
            (if (> (point-max) (- (frame-width) wid))
                (overlay-put o 'after-string (concat "\n" echo-bar-text))
              (overlay-put o 'after-string echo-bar-text)))))

      ;; Display the text in Minibuf-0
      (with-current-buffer " *Minibuf-0*"
        (delete-region (point-min) (point-max))
        (insert echo-bar-text)))))

(defun echo-bar-update ()
  "Udpate the text in the echo bar."
  (echo-bar-set-text
   (mapconcat
    'identity
    (cl-remove-if
     #'(lambda (n) (eq (length n) 0))
     (mapcar #'(lambda (mod) (ignore-errors (funcall mod))) echo-bar-modules))
    echo-bar-separator)))

(defun echo-bar-enable ()
  "Enable echo-bar."
  (echo-bar-disable)

  ;; Create overlays in each echo area buffer
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (remove-overlays (point-min) (point-max))
      (push (make-overlay (point-min) (point-max) nil nil t)
            echo-bar-overlays)))

  ;; Start the timer to automatically update
  (when echo-bar-update-interval
    (run-with-timer 0 echo-bar-update-interval 'echo-bar-update))

  ;; Add the setup function to the minibuffer hook
  (add-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup))

(defun echo-bar-disable ()
  "Disable echo-bar."
  ;; Remove echo-bar overlays
  (mapc 'delete-overlay echo-bar-overlays)
  (setq echo-bar-overlays nil)

  ;; Remove text from Minibuf-0
  (with-current-buffer " *Minibuf-0*"
    (delete-region (point-min) (point-max)))

  ;; Cancel the update timer
  (cancel-function-timers #'echo-bar-update)

  ;; Remove the setup function from the minibuffer hook
  (remove-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup))

(define-minor-mode echo-bar-mode
  "Display text at end of the echo area."
  :global t
  (if echo-bar-mode
      (echo-bar-enable)
    (echo-bar-disable)))

(provide 'init-echo-bar)
;;; init-echo-bar.el ends here
