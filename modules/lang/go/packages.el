;;; lang/go/packages.el -*- lexical-binding: t; -*-

(package! go-mode)

(when (modulep! :completion company)
  (package! company-go))
