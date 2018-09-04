;;; core-editor.el --- Initialize core editor configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/.emacs.d

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

(setq-default
 large-file-warning-threshold 30000000
 ;; Bookmarks
 bookmark-default-file (concat dotemacs-cache-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; Whitespace
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(after-load! 'abbrev
  (diminish 'abbrev-mode))
(after-load! 'eldoc
  (diminish 'eldoc-mode))
(after-load! 'simple
  (diminish 'auto-fill-function)
  (diminish 'visual-line-mode))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;
;; Built-in plugins
;;

;; revert buffers for changed files
(use-package autorevert
  :hook (find-file . global-auto-revert-mode)
  :diminish auto-revert-mode
  :config
  (setq auto-revert-verbose nil))

;; persist variables across sessions
(use-package savehist
  :hook (post-command . savehist-mode)
  :config
  (setq savehist-file (concat dotemacs-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

;; persistent point location in buffers
(use-package saveplace
  :hook ((find-file dired-initial-position) . save-place-mode)
  :config
  (setq save-place-file (concat dotemacs-cache-dir "saveplace")))

;; Hideshow
(use-package hideshow
  :diminish hs-minor-mode
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding)))

;; recent files
(use-package recentf
  :defer 1
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat dotemacs-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-auto-cleanup 'never
        recentf-exclude (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                              "^/var/folders/.+$" dotemacs-cache-dir dotemacs-local-dir))
  (recentf-mode +1))

(use-package server
  :when (display-graphic-p)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'core-editor)
;;; core-editor.el ends here
