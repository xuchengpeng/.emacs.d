;;; dotemacs-themes-ext-treemacs.el --- description -*- lexical-binding: t; no-byte-compile: t -*-

(defgroup dotemacs-themes-treemacs nil
  "Options for dotemacs's treemacs theme"
  :group 'dotemacs-themes)


;;
;;; Variables

(defcustom dotemacs-themes-treemacs-enable-variable-pitch t
  "If non-nil, the labels for files, folders and projects are displayed with the
variable-pitch face."
  :type 'boolean
  :group 'dotemacs-themes-treemacs)

(defcustom dotemacs-themes-treemacs-line-spacing 1
  "Line-spacing for treemacs buffer."
  :type 'integer
  :group 'dotemacs-themes-treemacs)


;;
;;; Library

(defun dotemacs-themes-hide-fringes ()
  "Remove fringes in currnent window."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

(defun dotemacs-themes-setup-tab-width (&rest _)
  "Set `tab-width' to 1, so tab characters don't ruin formatting."
  (setq tab-width 1))

(defun dotemacs-themes-setup-line-spacing ()
  "Set `line-spacing' in treemacs buffers."
  (setq line-spacing dotemacs-themes-treemacs-line-spacing))

(defun dotemacs-themes-hide-modeline ()
  (setq mode-line-format nil))

(defun dotemacs-themes-enable-treemacs-variable-pitch-labels (&rest _)
  (when dotemacs-themes-treemacs-enable-variable-pitch
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
                    treemacs-file-face
                    treemacs-tags-face))
      (let ((faces (face-attribute face :inherit nil)))
        (set-face-attribute
         face nil :inherit
         `(variable-pitch ,@(delq 'unspecified (if (listp faces) faces (list faces)))))))))

(defun dotemacs-themes-fix-treemacs-icons-dired-mode ()
  "Set `tab-width' to 1 in dired-mode if `treemacs-icons-dired-mode' is active."
  (if treemacs-icons-dired-mode
      (add-hook 'dired-mode-hook #'dotemacs-themes-setup-tab-width nil t)
    (remove-hook 'dired-mode-hook #'dotemacs-themes-setup-tab-width t)))

;;
;;; Bootstrap

(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook #'dotemacs-themes-setup-tab-width)
  (add-hook 'treemacs-mode-hook #'dotemacs-themes-setup-line-spacing)
  
  (add-hook 'treemacs-icons-dired-mode-hook #'dotemacs-themes-fix-treemacs-icons-dired-mode)

  ;; The modeline isn't useful in treemacs
  (add-hook 'treemacs-mode-hook #'dotemacs-themes-hide-modeline)

  ;; Disable fringes (and reset them everytime treemacs is selected because it
  ;; may change due to outside factors)
  (add-hook 'treemacs-mode-hook #'dotemacs-themes-hide-fringes)
  (advice-add #'treemacs-select-window :after #'dotemacs-themes-hide-fringes)

  ;; variable-pitch labels for files/folders
  (dotemacs-themes-enable-treemacs-variable-pitch-labels)
  (advice-add #'load-theme :after #'dotemacs-themes-enable-treemacs-variable-pitch-labels)

  (treemacs-load-theme "dotemacs"))

;;;###autoload
(defun dotemacs-themes-treemacs-config ()
  "Install dotemacs-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.")

(provide 'dotemacs-themes-ext-treemacs)
;;; dotemacs-themes-ext-treemacs.el ends here
