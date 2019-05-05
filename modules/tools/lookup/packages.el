;;; tools/lookup/packages.el -*- lexical-binding: t; -*-

(when (featurep! :completion ivy)
  (package! ivy-xref))
(when (featurep! :completion helm)
  (package! helm-xref))
