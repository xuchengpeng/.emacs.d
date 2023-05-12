;;; dotemacs-window-select.el --- Switch window.

(dotemacs-require-packages '(ace-window winum))

(use-package ace-window
  :defer t
  :init
  (global-set-key [remap other-window] 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winum
  :hook (window-selection-change-functions . winum-mode))

(provide 'dotemacs-window-select)
;;; dotemacs-window-select.el ends here