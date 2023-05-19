;;; dotemacs-tabs.el --- Tabs. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tabs configuration.
;;

;;; Code:

(dotemacs-require-packages '(centaur-tabs))

(use-package centaur-tabs
  :hook (find-file . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons nil
        centaur-tabs-style "bar"
        centaur-tabs-set-bar 'left
        centaur-tabs-set-close-button nil
        centaur-tabs-close-button "X"
        centaur-tabs-set-modified-marker nil
        centaur-tabs-modified-marker "•"
        centaur-tabs-cycle-scope 'tabs))

(provide 'dotemacs-tabs)
;;; dotemacs-tabs.el ends here
