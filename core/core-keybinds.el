;;; core-keybinds.el -*- lexical-binding: t; -*-

(defvar dotemacs-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar dotemacs-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar dotemacs-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar dotemacs-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(require 'general)

(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)

(general-create-definer map!
  :states nil
  :keymaps 'override
  :prefix dotemacs-leader-alt-key)

(general-create-definer map-local!
  :states nil
  :keymaps 'override
  :prefix dotemacs-localleader-alt-key)

(after-load! 'evil
  (general-create-definer map!
    :states '(normal visual insert emacs motion replace)
    :keymaps 'override
    :prefix dotemacs-leader-key
    :non-normal-prefix dotemacs-leader-alt-key)
  
  (general-create-definer map-local!
    :states '(normal visual insert emacs motion replace)
    :keymaps 'override
    :prefix dotemacs-localleader-key
    :non-normal-prefix dotemacs-localleader-alt-key))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
