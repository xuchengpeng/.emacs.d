;;; lang/org/packages.el -*- lexical-binding: t; -*-

(package! '(org-plus-contrib :type git :host github :repo "emacs-straight/org-mode"
                             :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")
                             :includes org))
(package! htmlize)

(when (modulep! +export)
  (package! ox-pandoc)
  (package! ox-hugo))
