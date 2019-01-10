;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-directory "~/org/"
        org-agenda-files (list org-directory))
  :config
  (setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE")))
  (setq org-log-done 'time
        org-startup-indented t
        org-startup-folded t
        org-clock-persist t
        org-clock-persist-file (concat dotemacs-cache-dir "org-clock-save.el")
        org-clock-in-resume t
        org-clock-into-drawer t
        org-log-into-drawer t)
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook #'auto-fill-mode))

(when (featurep! +capture)
  (load! "+capture"))
(when (featurep! +export)
  (load! "+export"))
(when (featurep! +publish)
  (load! "+publish"))
