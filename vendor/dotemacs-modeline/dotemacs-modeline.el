;;; dotemacs-modeline.el --- A minimal and modern mode-line -*- lexical-binding: t; -*-

;;; Code:

(require 'dotemacs-modeline-core)
(require 'dotemacs-modeline-segments)

;;
;; Mode lines
;;

(dotemacs-modeline-def-modeline 'main
  '(bar matches buffer-info-simple remote-host buffer-position selection-info)
  '(misc-info minor-modes buffer-encoding major-mode process vcs checker))

(dotemacs-modeline-def-modeline 'minimal
  '(bar matches " " buffer-info-simple)
  '(media-info major-mode))

(dotemacs-modeline-def-modeline 'special
  '(bar matches buffer-info-simple buffer-position selection-info)
  '(misc-info minor-modes buffer-encoding major-mode process checker))

(dotemacs-modeline-def-modeline 'project
  '(bar " " buffer-default-directory)
  '(misc-info " " major-mode process))

(dotemacs-modeline-def-modeline 'media
  '(bar buffer-size buffer-info-simple)
  '(misc-info media-info major-mode process vcs))

;;
;; Interfaces
;;

;;;###autoload
(defun dotemacs-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (dotemacs-modeline-set-modeline 'main default))

;;;###autoload
(defun dotemacs-modeline-set-minimal-modeline ()
  "Set minimal mode-line."
  (dotemacs-modeline-set-modeline 'minimal))

;;;###autoload
(defun dotemacs-modeline-set-special-modeline ()
  "Set sepcial mode-line."
  (dotemacs-modeline-set-modeline 'special))

;;;###autoload
(defun dotemacs-modeline-set-media-modeline ()
  "Set media mode-line."
  (dotemacs-modeline-set-modeline 'media))

;;;###autoload
(defun dotemacs-modeline-set-project-modeline ()
  "Set project mode-line."
  (dotemacs-modeline-set-modeline 'project))

;;
;; Mode
;;

(defvar dotemacs-modeline--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode dotemacs-modeline-mode
  "Toggle dotemacs-modeline on or off."
  :group 'dotemacs-modeline
  :global t
  :lighter nil
  (if dotemacs-modeline-mode
      (progn
        (dotemacs-modeline-refresh-bars)    ; create bars
        (dotemacs-modeline-set-main-modeline t) ; set default mode-line
        (unless after-init-time
          ;; These buffers are already created and don't get modelines. For the love
          ;; of Emacs, someone give the man a modeline!
          (dolist (bname '("*scratch*" "*Messages*"))
            (with-current-buffer bname
              (dotemacs-modeline-set-main-modeline))))
        ;; Add hooks
        (add-hook 'magit-mode-hook #'dotemacs-modeline-set-project-modeline)
        (add-hook 'dashboard-mode-hook #'dotemacs-modeline-set-project-modeline)
        (add-hook 'image-mode-hook #'dotemacs-modeline-set-media-modeline)
        (add-hook 'circe-mode-hook #'dotemacs-modeline-set-special-modeline))
    (progn
      ;; Restore mode-line
      (setq-default mode-line-format dotemacs-modeline--default-mode-line)
      ;; Remove hooks
      (remove-hook 'magit-mode-hook #'dotemacs-modeline-set-project-modeline)
      (remove-hook 'dashboard-mode-hook #'dotemacs-modeline-set-project-modeline)
      (remove-hook 'image-mode-hook #'dotemacs-modeline-set-media-modeline)
      (remove-hook 'circe-mode-hook #'dotemacs-modeline-set-special-modeline))))

(provide 'dotemacs-modeline)

;;; dotemacs-modeline.el ends here
