;;; dotemacs-themes-ext-visual-bell.el --- flash mode-line on error -*- lexical-binding: t; -*-

(defface dotemacs-visual-bell '((t (:inherit error :inverse-video t)))
  "Face to use for the mode-line when `dotemacs-themes-visual-bell-config' is used."
  :group 'dotemacs-themes)

(defvar dotemacs-themes--bell-p nil)
;;;###autoload
(defun dotemacs-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (unless dotemacs-themes--bell-p
    (let ((old-remap (copy-alist face-remapping-alist)))
      (setq dotemacs-themes--bell-p t)
      (setq face-remapping-alist
            (append (delete (assq 'mode-line face-remapping-alist)
                            face-remapping-alist)
                    '((mode-line dotemacs-visual-bell))))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (remap buf)
                        (with-current-buffer buf
                          (when (assq 'mode-line face-remapping-alist)
                            (setq face-remapping-alist remap
                                  dotemacs-themes--bell-p nil))
                          (force-mode-line-update)))
                      old-remap
                      (current-buffer)))))

;;;###autoload
(defun dotemacs-themes-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'dotemacs-themes-visual-bell-fn
        visible-bell t))

(provide 'dotemacs-themes-ext-visual-bell)
;;; dotemacs-themes-ext-visual-bell.el ends here
