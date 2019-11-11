;;; tools/utils/packages.el -*- lexical-binding: t; -*-

(when (memq window-system '(mac ns x))
 (package! exec-path-from-shell))
