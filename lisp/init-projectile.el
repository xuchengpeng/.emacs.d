;; init-projectile.el --- Initialize projectile configurations.
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
;; Projectile configurations.
;;

;;; Code:

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-directory "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-directory "projectile-bookmarks.eld"))
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))))

(provide 'init-projectile)

;;; init-projectile.el ends here
