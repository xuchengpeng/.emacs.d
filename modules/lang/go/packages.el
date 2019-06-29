;;; lang/go/packages.el -*- lexical-binding: t; -*-

(package! go-mode)

(when (featurep! :completion company)
  (package! company-go))
