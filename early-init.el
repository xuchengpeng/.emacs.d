;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(setq frame-inhibit-implied-resize t)

(push '(fullscreen . maximized) default-frame-alist)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;;; early-init.el ends here
