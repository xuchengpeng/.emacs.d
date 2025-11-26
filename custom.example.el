;;; custom.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "http_proxy" "http://user:password@proxy.server.com:port")
(setenv "https_proxy" "http://user:password@proxy.server.com:port")
(setenv "no_proxy" ".xxxx.com,.inner,localhost,127.0.0.1")

(setq user-full-name "Chuck"
      user-mail-address "xxxxxx@xxx.com")

(setq dotemacs-font "Iosevka SS14-13"
      dotemacs-cn-font "LXGW WenKai")

(setq dotemacs-package-archives 'custom) ;; melpa, tuna, custom
(setq package-check-signature nil
      package-archives
      '(("gnu"    . "/path/to/elpa-mirror/gnu/")
        ("nongnu" . "/path/to/elpa-mirror/nongnu/")
        ("melpa"  . "/path/to/elpa-mirror/melpa/")))

(with-eval-after-load 'gptel
  (setenv "GPTEL_GEMINI_KEY" "xxxxxxxxxxxxxxxxx")
  (setq gptel-curl-extra-args '("--insecure")))

(with-eval-after-load 'elfeed
  (setq elfeed-curl-extra-arguments '("--insecure")))

(with-eval-after-load 'init-ui
  (defun +init-theme-override ()
    (use-package modus-themes
      :disabled
      :ensure t)
    (use-package catppuccin-themes
      :vc (:url "https://github.com/xuchengpeng/catppuccin-themes")
      :config
      (catppuccin-themes-take-over-modus-themes-mode 1)
      (defun +catppuccin-themes-custom-faces (&rest _)
        (modus-themes-with-colors
          (custom-set-faces
           `(echo-bar-red-face ((,c :foreground ,red)))
           `(echo-bar-green-face ((,c :foreground ,green)))
           `(echo-bar-yellow-face ((,c :foreground ,yellow)))
           `(echo-bar-blue-face ((,c :foreground ,blue)))
           `(echo-bar-magenta-face ((,c :foreground ,magenta)))
           `(echo-bar-cyan-face ((,c :foreground ,cyan)))
           `(echo-bar-gray-face ((,c :foreground ,fg-dim))))))
      (add-hook 'modus-themes-after-load-theme-hook #'+catppuccin-themes-custom-faces)
      (modus-themes-load-theme 'catppuccin-latte)
      (setq modus-themes-to-toggle '(catppuccin-latte catppuccin-mocha))
      (keymap-global-set "<f5>" #'modus-themes-toggle)))
  (advice-add #'+init-theme :override #'+init-theme-override))

;;; custom.el ends here
