;;; dotemacs-themes-ext-neotree.el -*- lexical-binding: t; -*-

(defgroup dotemacs-themes-neotree nil
  "Options for dotemacs's neotree theme"
  :group 'dotemacs-themes)

(defcustom dotemacs-themes-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'dotemacs-themes-neotree)

(defcustom dotemacs-themes-neotree-enable-variable-pitch nil
  "If non-nil, labels will use the `dotemacs-themes-neotree-dir-face' and
`dotemacs-themes-neotree-dir-face' faces, which inherit from the `variable-pitch' face."
  :type 'boolean
  :group 'dotemacs-themes-neotree)

(defun dotemacs-themes--neotree-no-fringes ()
  "Remove fringes in neotree.
They are reset each time you select the neotree pane and highlighted
incorrectly, so remove them."
  (set-window-fringes neo-global--window 0 0))

(defun dotemacs-themes--neotree-setup (&rest _)
  (setq line-spacing dotemacs-themes-neotree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

(defun dotemacs-themes-neotree-insert-root (node)
  ;; insert project name
  (insert
   (propertize
    (concat (or (neo-path--file-short-name node) "-")
            "\n")
    'face `(:inherit ,(append (if dotemacs-themes-neotree-enable-variable-pitch '(variable-pitch))
                              '(neo-root-dir-face))))))

(with-eval-after-load 'neotree
  ;; Incompatible with this theme
  (setq neo-vc-integration nil)
  ;; Enable buffer-local hl-line and adjust line-spacing
  (add-hook 'neo-after-create-hook #'dotemacs-themes--neotree-setup)
  ;; Remove fringes in Neotree pane
  (advice-add #'neo-global--select-window :after #'dotemacs-themes--neotree-no-fringes)
  ;; Shorter pwd in neotree
  (advice-add #'neo-buffer--insert-root-entry :override #'dotemacs-themes-neotree-insert-root))

;;;###autoload
(defun dotemacs-themes-neotree-config ()
  "Install dotemacs-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.")

(provide 'dotemacs-themes-ext-neotree)
;;; dotemacs-themes-ext-neotree.el ends here
