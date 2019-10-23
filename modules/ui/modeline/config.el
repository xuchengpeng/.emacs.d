;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(use-package dotemacs-modeline
  :straight (dotemacs-modeline :type git :host github :repo "xuchengpeng/dotemacs-modeline")
  :hook (after-init . dotemacs-modeline-mode)
  :config
  (require 'diminish)
  (after! abbrev
    (diminish 'abbrev-mode "Abv"))
  (after! eldoc
    (diminish 'eldoc-mode))
  (after! simple
    (diminish 'auto-fill-function)
    (diminish 'visual-line-mode))
  (after! autorevert
    (diminish 'auto-revert-mode))
  (after! hideshow
    (diminish 'hs-minor-mode))
  (after! company
    (diminish 'company-mode))
  (after! undo-tree
    (diminish 'undo-tree-mode))
  (after! smartparens
    (diminish 'smartparens-mode))
  (after! which-key
    (diminish 'which-key-mode))
  (after! symbol-overlay
    (diminish 'symbol-overlay-mode))
  (after! persp-mode
    (diminish 'persp-mode))
  (after! yasnippet
    (diminish 'yas-minor-mode "Yas")))
