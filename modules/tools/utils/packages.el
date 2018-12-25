;;; tools/utils/packages.el -*- lexical-binding: t; -*-

(unless (fboundp 'display-line-numbers-mode)
  (package! nlinum)
  (package! nlinum-relative))

(package! hydra)

(when (memq window-system '(mac ns x))
 (package! exec-path-from-shell))
