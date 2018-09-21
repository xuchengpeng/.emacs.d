;; init-eshell.el --- Initialize eshell configurations.
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
;; Eshell configurations.
;;

;;; Code:

(use-package eshell
  :commands (eshell)
  :config
  (setq
       eshell-buffer-shorthand t
       eshell-history-size 1000
       eshell-save-history-on-exit t
       eshell-hist-ignoredups t
       eshell-cmpl-ignore-case t
       eshell-error-if-no-glob t
       eshell-glob-case-insensitive t
       eshell-scroll-to-bottom-on-input 'all
       eshell-directory-name (concat dotemacs-cache-directory "eshell")
       eshell-aliases-file (concat dotemacs-cache-directory "eshell/alias")
       )
  )

(use-package shell-pop
  :commands (shell-pop)
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom"))
  )

(provide 'init-eshell)

;;; init-eshell.el ends here
