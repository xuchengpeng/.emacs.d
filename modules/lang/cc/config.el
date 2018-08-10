;;; lang/cc/config.el

;; c
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq c-basic-offset 4
                  default-tab-width 4
                  tab-width 4
                  indent-tabs-mode nil)))
