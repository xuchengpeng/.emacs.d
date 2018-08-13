
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; maximized startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if (display-graphic-p)
                   (dotemacs-set-font))))

(if (display-graphic-p)
    (dotemacs-set-font))


(provide 'core-ui)
