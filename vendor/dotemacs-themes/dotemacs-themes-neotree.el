;;; dotemacs-themes-neotree.el -*- lexical-binding: t; -*-

(defgroup dotemacs-neotree nil
  "Options for dotemacs's neotree theme"
  :group 'dotemacs-themes)

;;
(defcustom dotemacs-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'dotemacs-neotree)

(defcustom dotemacs-neotree-enable-variable-pitch nil
  "If non-nil, labels will use the `variable-pitch' face."
  :type 'boolean
  :group 'dotemacs-neotree)


;;
(defun dotemacs--neotree-no-fringes ()
  "Remove fringes in neotree. They get reset each time you select the neotree
pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 0 0))

(defun dotemacs--neotree-setup (&rest _)
  (setq line-spacing dotemacs-neotree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

;;
(defun dotemacs-neotree-insert-root (node)
  ;; insert project name
  (insert
   (propertize
    (concat (or (neo-path--file-short-name node) "-")
            "\n")
    'face `(:inherit ,(append (if dotemacs-neotree-enable-variable-pitch '(variable-pitch))
                              '(neo-root-dir-face))))))

;;
(eval-after-load 'neotree
  (lambda ()
    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook #'dotemacs--neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add #'neo-global--select-window :after #'dotemacs--neotree-no-fringes)
    ;; Shorter pwd in neotree
    (advice-add #'neo-buffer--insert-root-entry :override #'dotemacs-neotree-insert-root)))

(provide 'dotemacs-themes-neotree)
;;; dotemacs-themes-neotree.el ends here
