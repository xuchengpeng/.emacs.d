;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold 268435456
      package-enable-at-startup nil)

;; One less file to load at startup
(setq site-run-file nil)
