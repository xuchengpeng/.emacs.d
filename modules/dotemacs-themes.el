;;; dotemacs-themes.el --- Themes. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun dotemacs-init-theme ()
  "Init theme."
  (load "tokyonight-theme" 'nomessage t)
  (load-theme 'tokyonight t))

(add-hook 'after-init-hook #'dotemacs-init-theme)

(defun dotemacs-load-theme ()
  "Load theme."
  (interactive)
  (let ((choice (completing-read
                  "Select theme: "
                  '("tokyonight-storm" "tokyonight-night" "tokyonight-moon" "tokyonight-day")))
        (tokyonight-prefix "tokyonight-"))
    (cond ((string-prefix-p tokyonight-prefix choice)
           (setq tokyonight-theme-style (intern (substring choice (length tokyonight-prefix))))
           (load-theme 'tokyonight t))
          (t
           (load-theme (intern choice) t)))))

(provide 'dotemacs-themes)
;;; dotemacs-themes.el ends here
