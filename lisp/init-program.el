;; init-program.el --- Initialize program configurations.
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
;; Program configurations.
;;

;;; Code:

;; turn on visual-line-mode
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook))
  (add-hook hook 'visual-line-mode))

;; c
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq c-basic-offset 4
                  default-tab-width 4
                  tab-width 4
                  indent-tabs-mode nil)))

;;shell
;; (dolist (exp '("\\.sh\\'" "\\.zsh\\'"
;;                "\\.bash_profile\\'" "\\.bash_history\\'"
;;                "\\.bash\\'" "\\.bashrc.local\\'" "\\.bashrc\\'"))
;;   (add-to-list 'auto-mode-alist
;;                (cons exp 'sh-mode)))
(dotemacs-add-auto-mode 'sh-mode
                        "\\.sh\\'" "\\.zsh\\'"
                        "\\.bash_profile\\'" "\\.bash_history\\'"
                        "\\.bash\\'" "\\.bashrc.local\\'" "\\.bashrc\\'")

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; web
(use-package web-mode
  :commands (web-mode)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))

  :config  
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-expanding t))

;;js
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :diminish (js2-mode . "JS2")
  :config
  (setq-default js2-basic-offset 2
                js2-basic-indent 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package json-mode
  :mode ("\\.json$" . json-mode))

;; toml
(use-package toml-mode
  :mode ("\\.toml$" . toml-mode))

;; yaml
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(provide 'init-program)

;;; init-program.el ends here
