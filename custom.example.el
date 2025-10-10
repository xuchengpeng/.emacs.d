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
  (setq gptel-use-curl nil))

(with-eval-after-load 'elfeed
  (setq elfeed-use-curl nil))

(use-package catppuccin-themes
  :vc (:url "https://github.com/xuchengpeng/catppuccin-themes")
  :init
  (setq +themes-to-toggle '(catppuccin-latte catppuccin-mocha))

  (defun +catppuccin-themes-custom-faces (&rest _)
    (catppuccin-themes-with-colors
      (custom-set-faces
       `(echo-bar-red-face ((,c :foreground ,red)))
       `(echo-bar-green-face ((,c :foreground ,green)))
       `(echo-bar-yellow-face ((,c :foreground ,yellow)))
       `(echo-bar-blue-face ((,c :foreground ,blue)))
       `(echo-bar-magenta-face ((,c :foreground ,mauve)))
       `(echo-bar-cyan-face ((,c :foreground ,teal)))
       `(echo-bar-gray-face ((,c :foreground ,overlay2))))))
  (add-hook 'enable-theme-functions #'+catppuccin-themes-custom-faces))

;;; custom.el ends here
