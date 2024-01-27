;;; init-dired.el --- Dired. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'dired-rsync)

(use-package dired
  :commands (dired dired-jump)
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask
        dired-listing-switches "-alh --group-directories-first"
        image-dired-dir (expand-file-name "image-dired" dotemacs-cache-dir)
        image-dired-db-file (expand-file-name "db.el" image-dired-dir)
        image-dired-gallery-dir (expand-file-name "gallery" image-dired-dir)
        image-dired-temp-image-file (expand-file-name "temp-image" image-dired-dir)
        image-dired-temp-rotate-image-file (expand-file-name "temp-rotate-image" image-dired-dir)
        image-dired-thumb-size 150)
  :config
  (keymap-set dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(use-package dired-aux
  :after (dired)
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

(use-package dired-rsync
  :after (dired)
  :config
  (keymap-set dired-mode-map "C-c C-r" #'dired-rsync))

(provide 'init-dired)
;;; init-dired.el ends here
