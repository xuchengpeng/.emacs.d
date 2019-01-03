;;; feature/syntax-checker/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +syntax-checker/toggle ()
  "Toggle flycheck or flymake mode."
  (interactive)
  (cond ((featurep! +flycheck)
         (if flycheck-mode
             (flycheck-mode -1)
           (flycheck-mode +1)))
        ((or (featurep! +flymake) t)
         (if flymake-mode
             (flymake-mode -1)
           (flymake-mode +1)))))
