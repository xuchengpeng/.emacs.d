;;; core/autoload/packages.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dotemacs/upgrade-packages ()
  "Upgrade installed packages."
  (interactive)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))
