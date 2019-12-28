;;; tools/utils/packages.el -*- lexical-binding: t; -*-

(package! helpful)
(when (memq window-system '(mac ns x))
 (package! exec-path-from-shell))
