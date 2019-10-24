;;; lang/org/packages.el -*- lexical-binding: t; -*-

(package! org-plus-contrib)

(when (featurep! +export)
  (package! ox-pandoc)
  (package! ox-hugo))

(when (featurep! +publish)
  (package! htmlize))
