;;; tools/package-manager/config.el -*- lexical-binding: t; -*-

(use-package paradox
  :commands paradox-list-packages)

(use-package package-utils
  :commands (package-utils-list-upgrade
             package-utils-upgrade-all))
