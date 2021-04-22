;;; early-init.el -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(fset #'package--ensure-init-file #'ignore)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun dotemacs-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'dotemacs-reset-file-handler-alist-h)))

(setq user-emacs-directory (file-name-directory load-file-name))

(load (concat user-emacs-directory "core/core") nil 'nomessage)
