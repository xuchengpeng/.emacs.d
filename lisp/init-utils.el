;; init-utils.el --- Initialize ultilities.
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
;; Some useful Utilities.
;;

;;; Code:

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :hook (after-init . which-key-mode))

;; Environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; filetree configurations
(use-package neotree
  :disabled
  :bind ("<f8>" . neotree-toggle))

(use-package treemacs
  :commands (treemacs)
  :bind ("<f8>" . treemacs)
  :config
  (setq treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name "treemacs-persist" dotemacs-cache-directory)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)
  
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package pt
  :disabled
  :commands (pt-regexp projectile-pt))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package hydra
  :bind (("C-c f" . hydra-flycheck/body)
         ("C-c t" . hydra-toggle/body)
         ("C-c w" . hydra-window/body))
  :init
  (defhydra hydra-flycheck (:color pink)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^--------^----------^------^------------^-------^----------
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            ^^
^^                  ^^                  ^^
"
    ("q" nil)
    ("<" flycheck-previous-error)
    (">" flycheck-next-error)
    ("?" flycheck-describe-checker :color blue)
    ("d" flycheck-disable-checker :color blue)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors :color blue)
    ("m" flycheck-manual :color blue)
    ("s" flycheck-select-checker :color blue)
    ("v" flycheck-verify-setup :color blue))
  (defhydra hydra-toggle (:color pink
                          :hint nil)
    "
_a_: abbrev-mode
_d_: debug-on-error
_f_: auto-fill-mode
_o_: org-mode
_t_: truncate-lines
_w_: whitespace-mode
_q_: Quit

    "
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("o" org-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil))
  (defhydra hydra-window (:color pink
                          :hint nil)
    "
               -- WINDOW MENU --
^^^^^^^^-----------------------------------------------------
^Move Point^     ^Move Splitter^  ^Split^
^^^^^^^^-----------------------------------------------------
_<up>_           _<S-up>_         _0_: delete-window
_<left>_         _<S-left>_       _1_: delete-other-windows
_<down>_         _<S-down>_       _2_: split-window-below
_<right>_        _<S-right>_      _3_: split-window-right
You can use arrow-keys or WASD.
_q_: Quit
    "
    ("0" delete-window :exit t)
    ("1" delete-other-windows :exit t)
    ("2" split-window-below :exit t)
    ("3" split-window-right :exit t)
    ("a" windmove-left nil)
    ("s" windmove-down nil)
    ("w" windmove-up nil)
    ("d" windmove-right nil)
    ("A" hydra-move-splitter-left nil)
    ("S" hydra-move-splitter-down nil)
    ("W" hydra-move-splitter-up nil)
    ("D" hydra-move-splitter-right nil)
    ("<left>" windmove-left nil)
    ("<down>" windmove-down nil)
    ("<up>" windmove-up nil)
    ("<right>" windmove-right nil)
    ("<S-left>" hydra-move-splitter-left nil)
    ("<S-down>" hydra-move-splitter-down nil)
    ("<S-up>" hydra-move-splitter-up nil)
    ("<S-right>" hydra-move-splitter-right nil)
    ("u" hydra--universal-argument nil)
    ("q" nil)))

(use-package winum
  :disabled
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9))
  :config
  (require 'winum)
  
  (defun winum-assign-9-to-calculator-8-to-flycheck-errors ()
    (cond
     ((equal (buffer-name) "*Calculator*") 9)
     ((equal (buffer-name) "*Flycheck errors*") 8)))
  
  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
  
  (add-to-list 'winum-assign-functions #'winum-assign-9-to-calculator-8-to-flycheck-errors)
  (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
  
  (set-face-attribute 'winum-face nil :weight 'bold)
  
  (setq window-numbering-scope            'global
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
        winum-assign-func                 'my-winum-assign-func
        winum-auto-setup-mode-line        t
        winum-mode-line-position          1
        winum-ignored-buffers             '(" *which-key*"))
  
  (winum-mode))

(use-package dashboard
  :disabled
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))

(provide 'init-utils)

;;; init-utils.el ends here
