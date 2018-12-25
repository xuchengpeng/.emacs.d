;;; core-keybinds.el -*- lexical-binding: t; -*-

(require 'general)

(defalias 'define-key! #'general-def)

(general-create-definer map!
  :states '(normal visual insert emacs motion replace)
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(general-create-definer map-local!
  :states '(normal visual insert emacs motion replace)
  :prefix "SPC m"
  :non-normal-prefix "M-SPC m")

(provide 'core-keybinds)
;;; core-keybinds.el ends here
