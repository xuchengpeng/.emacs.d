;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +flycheck)
       (use-package flycheck
         :hook (prog-mode . global-flycheck-mode)
         :commands (flycheck-list-errors flycheck-buffer)
         :config
         (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))))
      ((featurep! +flymake)
       (add-hook 'prog-mode-hook 'flymake-mode)))
