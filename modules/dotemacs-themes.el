;;; dotemacs-themes.el --- Themes. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun dotemacs-load-theme ()
  "Load theme after init."
  (load "tokyonight-theme" 'nomessage t)
  (load-theme 'tokyonight t))

(add-hook 'after-init-hook #'dotemacs-load-theme)

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here
