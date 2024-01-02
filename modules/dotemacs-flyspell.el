;;; dotemacs-flyspell.el --- flyspell. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flyspell
  :if (executable-find "aspell")
  :hook (text-mode . flyspell-mode)
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together")
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(provide 'dotemacs-flyspell)
;;; dotemacs-flyspell.el ends here
