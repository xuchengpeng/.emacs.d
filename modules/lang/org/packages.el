;;; lang/org/packages.el -*- lexical-binding: t; -*-

(package! org-plus-contrib)

(when (featurep! +export)
  (package! ox-pandoc
            ox-hugo))
