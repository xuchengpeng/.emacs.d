;;; dotemacs-themes-treemacs.el --- description -*- lexical-binding: t; -*-

(defgroup dotemacs-treemacs nil
  "Options for dotemacs's treemacs theme"
  :group 'dotemacs-themes)

(defcustom dotemacs-treemacs-enable-variable-pitch t
  "If non-nil, the labels for files, folders and projects are displayed with the
variable-pitch face."
  :type 'boolean
  :group 'dotemacs-treemacs)

(defcustom dotemacs-treemacs-line-spacing 1
  "Line-spacing for treemacs buffer."
  :type 'symbol
  :group 'dotemacs-treemacs)

(defun dotemacs--treemacs-no-fringes ()
  "Remove fringes in treemacs. They get reset each time you select the neotree
pane and are highlighted incorrectly when used with `solaire-mode'."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

(defun dotemacs--treemacs-setup (&rest _)
  (setq line-spacing dotemacs-treemacs-line-spacing
        tab-width 1))

(defun dotemacs--treemacs-hide-modeline ()
  (setq mode-line-format nil))

(defun dotemacs--treemacs-variable-pitch-labels (&rest _)
  (when dotemacs-treemacs-enable-variable-pitch
    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face))
      (let ((faces (face-attribute face :inherit nil)))
        (set-face-attribute
         face nil :inherit
         `(variable-pitch ,@(delq 'unspecified (if (listp faces) faces (list faces)))))))))

(eval-after-load 'treemacs
  (lambda ()
    (add-hook 'treemacs-mode-hook #'dotemacs--treemacs-setup)
    (add-hook 'treemacs-mode-hook #'dotemacs--treemacs-hide-modeline)

    ;; no fringes in treemacs window
    (add-hook 'treemacs-mode-hook #'dotemacs--treemacs-no-fringes)
    (advice-add #'treemacs-select-window :after #'dotemacs--treemacs-no-fringes)

    ;; variable-pitch labels for files/folders
    (dotemacs--treemacs-variable-pitch-labels)
    (advice-add #'load-theme :after #'dotemacs--treemacs-variable-pitch-labels)))

(provide 'dotemacs-themes-treemacs)
;;; dotemacs-themes-treemacs.el ends here
