;;; custom.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "http_proxy" "http://user:password@proxy.server.com:port")
(setenv "https_proxy" "http://user:password@proxy.server.com:port")
(setenv "no_proxy" ".xxxx.com,.inner,localhost,127.0.0.1")

(setq user-full-name "Chuck"
      user-mail-address "xxxxxx@xxx.com")

(setq dotemacs-font "JetBrainsMono NF-13.5"
      dotemacs-cn-font "LXGW WenKai Mono"
      dotemacs-symbol-font "Segoe UI Symbol"
      dotemacs-emoji-font "Segoe UI Emoji")

(setq dotemacs-package-archives nil) ;; melpa, tuna, nil
(setq package-check-signature nil
      package-archives
      '(("gnu"    . "/path/to/elpa-mirror/gnu/")
        ("nongnu" . "/path/to/elpa-mirror/nongnu/")
        ("melpa"  . "/path/to/elpa-mirror/melpa/")))

(with-eval-after-load 'gptel
  (setq gptel-curl-extra-args '("--insecure")))

(with-eval-after-load 'elfeed
  (setq elfeed-curl-extra-arguments '("--insecure")))

(with-eval-after-load 'init-ui
  (defun +init-theme-override ()
    (use-package catppuccin-themes
      :vc (:url "https://github.com/xuchengpeng/catppuccin-themes")
      :config
      (defun +themes-custom-faces (&rest _)
        (catppuccin-themes-with-colors
          (custom-set-faces
           `(+modeline-emphasis-face ((t :foreground ,sky)))
           `(+modeline-buffer-path-face ((t :foreground ,blue :weight bold)))
           `(+modeline-buffer-modified-face ((t :foreground ,yellow :weight bold)))
           `(+modeline-buffer-major-mode-face ((t :foreground ,blue :weight bold)))
           `(+modeline-vc-face ((t :foreground ,green :weight bold)))
           `(+modeline-error-face ((t :foreground ,red :weight bold)))
           `(+modeline-warning-face ((t :foreground ,yellow :weight bold)))
           `(+modeline-info-face ((t :foreground ,teal :weight bold)))
           `(echo-bar-red-face ((t :foreground ,red)))
           `(echo-bar-green-face ((t :foreground ,green)))
           `(echo-bar-yellow-face ((t :foreground ,yellow)))
           `(echo-bar-blue-face ((t :foreground ,blue)))
           `(echo-bar-magenta-face ((t :foreground ,mauve)))
           `(echo-bar-cyan-face ((t :foreground ,sky)))
           `(echo-bar-gray-face ((t :foreground ,subtext0))))))
      (add-hook 'catppuccin-themes-after-load-theme-hook #'+themes-custom-faces)
      (catppuccin-themes-load-theme 'catppuccin-latte)
      (keymap-global-set "<f5>" #'catppuccin-themes-toggle)))
  (advice-add #'+init-theme :override #'+init-theme-override))

;;; custom.el ends here
