;;; dotemacs-modules.el --- A listing of modules to load on startup. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when init-file-debug
  (require 'dotemacs-benchmark))
(require 'dotemacs-keybinds)
(require 'dotemacs-themes)
(require 'dotemacs-dired)
(require 'dotemacs-dashboard)
(require 'dotemacs-editing)
(require 'dotemacs-completion)
(require 'dotemacs-modeline)
(require 'dotemacs-highlight)
(require 'dotemacs-eglot)
(require 'dotemacs-programming)
(require 'dotemacs-utils)
(require 'dotemacs-eshell)
(require 'dotemacs-lang)

(provide 'dotemacs-modules)
;;; dotemacs-modules.el ends here
