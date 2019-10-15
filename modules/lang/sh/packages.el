;;; lang/sh/packages.el -*- lexical-binding: t; -*-

(when (featurep! :completion company)
  (package! company-shell))
