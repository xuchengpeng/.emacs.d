;; core-editor.el --- Initialize core editor configurations.
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Core editor configurations.
;;

;;; Code:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq user-full-name    dotemacs-full-name
      user-mail-address dotemacs-mail-address)

(setq-default indent-tabs-mode nil
              tab-width 4)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq ring-bell-function 'ignore)

;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 1001
      scroll-margin 0
      scroll-preserve-screen-position t)

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(setq-default major-mode 'text-mode)

(setq-default
  make-backup-files nil
  auto-save-default nil
  auto-save-list-file-name (concat dotemacs-cache-dir "autosave")
  backup-directory-alist   (list (cons "." (concat dotemacs-cache-dir "backup/"))))

(setq bookmark-default-file  (concat dotemacs-cache-dir "bookmarks")
      bookmark-save-flag     1)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(dotemacs-after-load 'abbrev
  (diminish 'abbrev-mode))
(dotemacs-after-load 'eldoc
  (diminish 'eldoc-mode))
(dotemacs-after-load 'autorevert
  (diminish 'auto-revert-mode))
(dotemacs-after-load 'simple
              (diminish 'auto-fill-function)
              (diminish 'visual-line-mode))

;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

;; Highlight the current line
(add-hook 'after-init-hook #'global-hl-line-mode)

;; Highlight matching paren
(add-hook 'after-init-hook #'show-paren-mode)
;; (setq show-paren-style 'expression)

(when (fboundp 'winner-mode)
  (add-hook 'after-init-hook #'winner-mode))

(use-package dired
  :defer t
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

;; Hideshow
(use-package hideshow
  :diminish hs-minor-mode
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding)))

;; recent files
(use-package recentf
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat dotemacs-cache-dir "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never
        recentf-exclude (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                              "^/var/folders/.+$" "/.cache/"))
  (recentf-mode +1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'core-editor)

;;; core-editor.el ends here
