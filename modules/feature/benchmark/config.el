;;; config.el --- Initialize benchmark. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Benchmark configurations.
;;

;;; Code:

(defun dotemacs/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar dotemacs/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around dotemacs/build-require-times (feature &optional filename noerror) activate)
  "Note in `dotemacs/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (dotemacs/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'dotemacs/require-times
                       (list feature require-start-time time)
                       t))))))

(define-derived-mode dotemacs/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 dotemacs/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 dotemacs/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'dotemacs/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun dotemacs/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun dotemacs/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun dotemacs/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in dotemacs/require-times
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (dotemacs/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun dotemacs/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (dotemacs/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun dotemacs/show-init-time ()
  (message "Emacs init completed in %.2fms"
           (dotemacs/time-subtract-millis (current-time) before-init-time)))

(add-hook 'dotemacs-post-init-hook 'dotemacs/show-init-time)
