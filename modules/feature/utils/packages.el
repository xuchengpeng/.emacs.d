;;; feature/utils/packages.el -*- lexical-binding: t; -*-

(unless (fboundp 'display-line-numbers-mode)
  (package! nlinum
            nlinum-relative))

(package! which-key
          hydra)

(when (memq window-system '(mac ns x))
 (package! exec-path-from-shell))
