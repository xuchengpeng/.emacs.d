;;; dotemacs-flyspell.el --- flyspell. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'flyspell-correct)

(use-package flyspell
  :defer t
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (flyspell-mode +1))

(use-package flyspell-correct
  :commands (flyspell-correct-at-point flyspell-correct-wrapper)
  :config
  (global-set-key [remap ispell-word] #'flyspell-correct-at-point))

(provide 'dotemacs-flyspell)
;;; dotemacs-flyspell.el ends here
