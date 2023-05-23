;;; dotemacs-benchmark.el --- benchmark. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dotemacs-time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar dotemacs-require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun dotemacs-require-times-wrapper (orig feature &rest args)
  "Note in `dotemacs-require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (dotemacs-time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'dotemacs-require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'dotemacs-require-times-wrapper)

(define-derived-mode dotemacs-require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 dotemacs-require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 dotemacs-require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'dotemacs-require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun dotemacs-require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun dotemacs-require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun dotemacs-require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in dotemacs-require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (dotemacs-time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun dotemacs-require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (dotemacs-require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun dotemacs-display-init-time (&optional return-p)
  (let ((package-count 0))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
    (funcall (if return-p #'format #'message)
             "dotemacs loaded %d packages in %.2fms"
             package-count
             (dotemacs-time-subtract-millis after-init-time before-init-time))))

(provide 'dotemacs-benchmark)
;;; dotemacs-benchmark.el ends here
