;;; dotemacs-modeline-core.el --- The core libraries for dotemacs-modeline -*- lexical-binding: t; -*-

;;
;; Variables
;;

(defvar dotemacs-modeline-height 25
  "How tall the mode-line should be (only respected in GUI Emacs).")

(defvar dotemacs-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI Emacs).")

(defvar dotemacs-modeline-minor-modes nil
  "Whether display minor modes in mode-line or not.")

(defvar dotemacs-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline segment.")

;;
;; Custom faces
;;

(defgroup dotemacs-modeline nil
  "dotemacs mode-line faces."
  :group 'faces)

(defface dotemacs-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path.")

(defface dotemacs-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path.")

(defface dotemacs-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defface dotemacs-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line.")

(defface dotemacs-modeline-buffer-minor-mode
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the minor-modes segment in the mode-line.")

(defface dotemacs-modeline-project-parent-dir
  '((t (:inherit (font-lock-comment-face bold))))
  "Face used for the project parent directory of the mode-line buffer path.")

(defface dotemacs-modeline-project-dir
  '((t (:inherit (font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path.")

(defface dotemacs-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path.")

(defface dotemacs-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface dotemacs-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `anzu', `evil-substitute'
  and`iedit', etc.")

(defface dotemacs-modeline-debug
  `((t (:inherit font-lock-doc-face)))
  "Face for debug-level messages in the modeline. Used by `*flycheck'.")

(defface dotemacs-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface dotemacs-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface dotemacs-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'")

(defface dotemacs-modeline-unread-number
  `((t (:inherit italic)))
  "Face for unread number in the modeline. Used by `github', `mu4e', etc.")

(defface dotemacs-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface dotemacs-modeline-inactive-bar `((t (:background ,(face-foreground 'mode-line-inactive))))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface dotemacs-modeline-evil-emacs-state '((t (:inherit dotemacs-modeline-warning)))
  "Face for the Emacs state tag in evil state indicator.")

(defface dotemacs-modeline-evil-insert-state '((t (:inherit dotemacs-modeline-urgent)))
  "Face for the insert state tag in evil state indicator.")

(defface dotemacs-modeline-evil-motion-state '((t :inherit dotemacs-modeline-buffer-path))
  "Face for the motion state tag in evil state indicator.")

(defface dotemacs-modeline-evil-normal-state '((t (:inherit dotemacs-modeline-info)))
  "Face for the normal state tag in evil state indicator.")

(defface dotemacs-modeline-evil-operator-state '((t (:inherit dotemacs-modeline-buffer-path)))
  "Face for the operator state tag in evil state indicator.")

(defface dotemacs-modeline-evil-visual-state '((t (:inherit dotemacs-modeline-buffer-file)))
  "Face for the visual state tag in evil state indicator.")

(defface dotemacs-modeline-evil-replace-state '((t (:inherit dotemacs-modeline-buffer-modified)))
  "Face for the replace state tag in evil state indicator.")

(defface dotemacs-modeline-persp-name '((t (:inherit (font-lock-comment-face italic))))
  "Face for the replace state tag in evil state indicator.")

(defface dotemacs-modeline-persp-buffer-not-in-persp '((t (:inherit (font-lock-doc-face bold italic))))
  "Face for the replace state tag in evil state indicator.")

;;
;; Externals
;;

(declare-function face-remap-remove-relative 'face-remap)
(declare-function project-roots 'project)
(declare-function projectile-project-root 'projectile)

;;
;; Modeline library
;;

(eval-and-compile
  (defvar dotemacs-modeline-fn-alist ())
  (defvar dotemacs-modeline-var-alist ()))

(defmacro dotemacs-modeline-def-segment (name &rest body)
  "Defines a modeline segment NAME with BODY and byte compiles it."
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
              (fset ',sym (lambda () ,docstring ,@body))
              (add-to-list 'dotemacs-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (byte-compile #',sym))))))))

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
  "Defines a modeline format and byte-compiles it.
  NAME is a symbol to identify it (used by `dotemacs-modeline' for retrieval).
  LHS and RHS are lists of symbols of modeline segments defined with
  `dotemacs-modeline-def-segment'.

  Example:
  (dotemacs-modeline-def-modeline 'minimal
    '(bar matches \" \" buffer-info)
    '(media-info major-mode))
  (dotemacs-modeline-set-modeline 'minimal t)"
  (let ((sym (intern (format "dotemacs-modeline-format--%s" name)))
        (lhs-forms (dotemacs-modeline--prepare-segments lhs))
        (rhs-forms (dotemacs-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (list lhs-forms
              (propertize
               " "
               'face (if (dotemacs-modeline--active) 'mode-line 'mode-line-inactive)
               'display `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 1 (string-width
                                                     (format-mode-line
                                                      (cons "" rhs-forms))))))))
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
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))

;;
;; Plugins
;;

;; Keep `dotemacs-modeline-current-window' up-to-date
(defun dotemacs-modeline--get-current-window ()
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defvar dotemacs-modeline-current-window (dotemacs-modeline--get-current-window))
(defun dotemacs-modeline-set-selected-window (&rest _)
  "Set `dotemacs-modeline-current-window' appropriately."
  (when-let ((win (dotemacs-modeline--get-current-window)))
    (unless (minibuffer-window-active-p win)
      (setq dotemacs-modeline-current-window win)
      (force-mode-line-update))))

(defun dotemacs-modeline-unset-selected-window ()
  "Unset `dotemacs-modeline-current-window' appropriately."
  (setq dotemacs-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'dotemacs-modeline-set-selected-window)
(add-hook 'buffer-list-update-hook #'dotemacs-modeline-set-selected-window)
(advice-add #'handle-switch-frame :after #'dotemacs-modeline-set-selected-window)
(advice-add #'make-frame :after #'dotemacs-modeline-set-selected-window)
(advice-add #'delete-frame :after #'dotemacs-modeline-set-selected-window)
(with-no-warnings
  (cond ((not (boundp 'after-focus-change-function))
         (add-hook 'focus-in-hook #'dotemacs-modeline-set-selected-window)
         (add-hook 'focus-out-hook #'dotemacs-modeline-unset-selected-window))
        ((defun dotemacs-modeline-refresh-frame ()
           (setq dotemacs-modeline-current-window nil)
           (cl-loop for frame in (frame-list)
                    if (eq (frame-focus-state frame) t)
                    return (setq dotemacs-modeline-current-window (frame-selected-window frame)))
           (force-mode-line-update))
         (add-function :after after-focus-change-function #'dotemacs-modeline-refresh-frame))))

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar dotemacs-modeline-remap-face-cookie nil)
(defun dotemacs-modeline-focus ()
  "Focus mode-line."
  (when dotemacs-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative dotemacs-modeline-remap-face-cookie)))
(defun dotemacs-modeline-unfocus ()
  "Unfocus mode-line."
  (setq dotemacs-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun dotemacs-modeline-focus-change ()
          (if (frame-focus-state)
              (dotemacs-modeline-focus)
            (dotemacs-modeline-unfocus)))
        (add-function :after after-focus-change-function #'dotemacs-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'dotemacs-modeline-focus)
      (add-hook 'focus-out-hook #'dotemacs-modeline-unfocus))))

;;
;; Modeline helpers
;;

(defvar dotemacs-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "Text style with icons in mode-line.")

(defvar dotemacs-modeline-inactive-vspc
  (propertize " " 'face '(:inherit (variable-pitch mode-line-inactive)))
  "Text style with icons in inactive mode-line.")

(defun dotemacs-modeline--active ()
  "Whether is an active window."
  (eq (selected-window) dotemacs-modeline-current-window))

(defvar-local dotemacs-modeline-project-root nil)
(defun dotemacs-modeline-project-root ()
  "Get the path to the root of your project.
  Return `default-directory' if no project was found."
  (or dotemacs-modeline-project-root
      (setq dotemacs-modeline-project-root
            (file-local-name
             (or
              (when (bound-and-true-p projectile-mode)
                (ignore-errors (projectile-project-root)))
              (when (fboundp 'project-current)
                (ignore-errors
                  (when-let ((project (project-current)))
                    (expand-file-name (car (project-roots project))))))
              default-directory)))))

(defun dotemacs-modeline--make-xpm (face width height)
  "Create an XPM bitmap via FACE, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
  'xpm t :ascent 'center)))))

(provide 'dotemacs-modeline-core)

;;; dotemacs-modeline-core.el ends here
