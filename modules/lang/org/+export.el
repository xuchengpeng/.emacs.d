;;; lang/org/+export.el -*- lexical-binding: t; -*-

(use-package ox-pandoc
  :after ox
  :config
  (setq org-export-directory (concat org-directory ".export/")
        org-export-backends '(ascii html latex md))
  
  (when (executable-find "pandoc")
    (add-to-list 'org-export-backends 'pandoc nil #'eq))
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)))
  
  ;; Export to a central location by default or if target isn't in
  ;; `org-directory'.
  (defun +org*export-output-file-name (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `org-directory'."
    (when (and (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p buffer-file-name org-directory))
      (cl-destructuring-bind (extension &optional subtreep _pubdir) args
        (let ((dir org-export-directory))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (setq args (list extension subtreep dir)))))
    args)
  (advice-add #'org-export-output-file-name :filter-args #'+org*export-output-file-name))

(use-package ox-hugo
  :after ox)
