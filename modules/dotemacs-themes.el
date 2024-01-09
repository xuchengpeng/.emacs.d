;;; dotemacs-themes.el --- Themes. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'modus-themes)

;;;###autoload
(defun dotemacs-init-theme ()
  "Init theme."
  (require 'modus-themes)
  (load-theme 'modus-operandi :no-confirm)

  (add-to-list 'load-path (expand-file-name "tokyonight-themes/" dotemacs-modules-dir))
  (require 'tokyonight-themes))

(add-hook 'after-init-hook #'dotemacs-init-theme)

(defun dotemacs-load-theme ()
  "Load theme."
  (interactive)
  (let ((choice (completing-read
                  "Select theme: "
                  '("tokyonight-storm" "tokyonight-night" "tokyonight-moon" "tokyonight-day"
                    "modus-operandi" "modus-vivendi"))))
    (consult-theme (intern choice))))

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here
