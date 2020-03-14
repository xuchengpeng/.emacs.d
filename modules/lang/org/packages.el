;;; lang/org/packages.el -*- lexical-binding: t; -*-

(package! org-plus-contrib)
(package! htmlize)

(when (featurep! +export)
  (package! ox-pandoc)
  (package! ox-hugo))
