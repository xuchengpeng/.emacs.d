;;; lang/org/packages.el -*- lexical-binding: t; -*-

(package! '(org-mode :type git :host github :repo "emacs-straight/org-mode"
                     :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")))
(package! htmlize)

(when (featurep! +export)
  (package! ox-pandoc)
  (package! ox-hugo))
