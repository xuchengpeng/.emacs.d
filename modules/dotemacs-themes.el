;;; dotemacs-themes.el --- Themes. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'modus-themes)

;;;###autoload
(defun dotemacs-init-theme ()
  "Init theme."
  (require 'modus-themes)
  (load-theme 'modus-operandi :no-confirm))

(add-hook 'after-init-hook #'dotemacs-init-theme)

(defun dotemacs-load-theme ()
  "Load theme."
  (interactive)
  (let ((choice (completing-read
                  "Select theme: "
                  '("tokyonight-storm" "tokyonight-night" "tokyonight-moon" "tokyonight-day"
                    "modus-operandi" "modus-vivendi")))
        (tokyonight-prefix "tokyonight-"))
    (cond ((string-prefix-p tokyonight-prefix choice)
           (mapc #'disable-theme custom-enabled-themes)
           (load "tokyonight-theme" 'nomessage t)
           (setq tokyonight-theme-style (intern (substring choice (length tokyonight-prefix))))
           (load-theme 'tokyonight :no-confirm))
          (t
           (consult-theme (intern choice))))))

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here
