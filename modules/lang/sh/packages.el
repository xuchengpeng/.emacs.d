;;; lang/sh/packages.el -*- lexical-binding: t; -*-

(when (modulep! :completion company)
  (package! company-shell))
