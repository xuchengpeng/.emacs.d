;;; feature/evil/+bindings.el -*- lexical-binding: t; -*-

(evil-define-key nil 'global
  (kbd "M-/") 'company-complete
  (kbd "C-c C-y") 'company-yasnippet)
(evil-define-key nil company-active-map
  (kbd "C-p") 'company-select-previous
  (kbd "C-n") 'company-select-next)
