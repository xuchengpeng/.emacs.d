;;; dotemacs-window-select.el --- Switch window. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'ace-window)

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window))

(provide 'dotemacs-window-select)
;;; dotemacs-window-select.el ends here