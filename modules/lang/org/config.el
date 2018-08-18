;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.txt$" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-directory (concat dotemacs-dir "org/")
        org-agenda-files (list org-directory))
  (setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE")))
  (setq org-log-done 'time
        org-startup-indented t
        org-startup-folded nil
        org-clock-persist t
        org-clock-persist-file (concat dotemacs-cache-dir "org-clock-save.el")
        org-clock-in-resume t
        org-clock-into-drawer t
        org-log-into-drawer t)
  (org-clock-persistence-insinuate))

(when (featurep! +capture)
  (load (expand-file-name "+capture.el" (DIR!)) t (not dotemacs-debug-mode)))
(when (featurep! +export)
  (load (expand-file-name "+export.el" (DIR!)) t (not dotemacs-debug-mode)))
