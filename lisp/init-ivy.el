;; init-ivy.el --- Initialize ivy configurations.
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
;; Ivy configurations.
;;

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial-or-done)
         ("RET" . ivy-alt-done))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'full
        ivy-magic-tilde nil
        ;;ivy-dynamic-exhibit-delay-ms 150
        ivy-count-format "(%d/%d) "
        ivy-format-function #'ivy-format-function-line
        projectile-completion-system 'ivy)
  ;; Integration with `magit'
  (dotemacs-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package ivy-hydra
  :after (ivy hydra))

;; ;; Package `prescient' is a library for intelligent sorting and
;; ;; filtering in various contexts.
;; (use-package prescient
;;   :defer t
;;   :config
;;   (setq prescient-filter-method 'regexp
;;         prescient-save-file (concat dotemacs-cache-directory "prescient-save.el"))
;;   ;; Remember usage statistics across Emacs sessions.
;;   (prescient-persist-mode +1))
;; 
;; ;; Package `ivy-prescient' provides intelligent sorting and filtering
;; ;; for candidates in Ivy menus.
;; (use-package ivy-prescient
;;   :demand t
;;   :after ivy
;;   :config
;;   (setq ivy-prescient-retain-classic-highlighting t)
;;   ;; Use `prescient' for Ivy menus.
;;   (ivy-prescient-mode +1))

(use-package swiper
  :after ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :bind (("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("M-i"     . counsel-grep-or-swiper)
         ("C-x r b" . counsel-bookmark)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable))
  :config
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))
  (let ((command
         (cond
          ((executable-find "rg")
           "rg --smart-case --no-heading --line-number --color never %s %s")
          ((executable-find "ag")
           "ag --smart-case --noheading --nocolor --numbers %s %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))
  (setq counsel-rg-base-command "rg --smart-case --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag --smart-case --nocolor --nogroup --numbers %s")
  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")))

(use-package counsel-projectile
  :disabled
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

;; (use-package smex
;;   :defer t
;;   :config
;;   (setq smex-history-length 10
;;         smex-save-file (concat dotemacs-cache-directory ".smex-items")))

(use-package amx
  :defer t
  :config
  (setq amx-history-length 10
        amx-save-file (concat dotemacs-cache-directory "amx-items")))

(provide 'init-ivy)

;;; init-ivy.el ends here
