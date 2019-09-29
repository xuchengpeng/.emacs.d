;;; core-editor.el --- Initialize core editor configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/.emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Core editor configurations.
;;

;;; Code:

;;
;;; File handling

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun dotemacs-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t)))))

;; Don't autosave files or create lock/history/backup files. The
;; editor doesn't need to hold our hands so much. We'll rely on git
;; and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name (concat dotemacs-cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat dotemacs-cache-dir "backup/"))))

;;
;;; Formatting

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 120)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")  ; for :retab

;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)

(after-load! 'abbrev
  (diminish 'abbrev-mode))
(after-load! 'eldoc
  (diminish 'eldoc-mode))
(after-load! 'simple
  (diminish 'auto-fill-function)
  (diminish 'visual-line-mode))

;;
;; Built-in plugins

(after-load! 'bookmark
  (setq bookmark-save-flag t))

;; revert buffers for changed files
(use-package autorevert
  :hook (find-file . global-auto-revert-mode)
  :diminish auto-revert-mode
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil))

;; persist variables across sessions
(use-package savehist
  :hook (post-command . savehist-mode)
  :config
  (setq savehist-file (concat dotemacs-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1))

;; persistent point location in buffers
(use-package saveplace
  :hook ((find-file dired-initial-position) . save-place-mode)
  :config
  (setq save-place-file (concat dotemacs-cache-dir "saveplace")
        save-place-limit 100)
  (defadvice! dotemacs--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! dotemacs--dont-prettify-saveplace-cache-a (orig-fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so we replace calls to `pp' with the much faster `prin1'."
    :around #'save-place-alist-to-file
    (cl-letf (((symbol-function #'pp)
               (symbol-function #'prin1)))
      (funcall orig-fn)))
  
  (save-place-mode +1))

;; Hideshow
(use-package hideshow
  :diminish hs-minor-mode
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode))

;; recent files
(use-package recentf
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat dotemacs-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 200
        recentf-auto-cleanup 'never
        recentf-exclude (list "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
                              "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
                              (lambda (path)
                                (ignore-errors (file-in-directory-p path dotemacs-local-dir)))
                              (lambda (path)
                                (ignore-errors (file-in-directory-p path dotemacs-cache-dir)))))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (quiet! (recentf-mode +1)))

(use-package server
  :when (display-graphic-p)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'core-editor)
;;; core-editor.el ends here
