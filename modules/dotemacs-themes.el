;;; dotemacs-themes.el --- Themes.

(dotemacs-require-packages '(doom-themes))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here