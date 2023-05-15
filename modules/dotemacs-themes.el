;;; dotemacs-themes.el --- Themes.

;;; Commentary:
;;
;; Themes configuration.
;;

;;; Code:

(dotemacs-require-package `(doom-themes :local-repo "doom-themes"))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here