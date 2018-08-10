;;; feature/utils/packages.el

(package! which-key
          hydra)

(when (memq window-system '(mac ns x))
 (package! exec-path-from-shell))
