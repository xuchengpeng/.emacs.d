# Chuck's Emacs Configuration with [use-package](https://github.com/jwiegley/use-package)

[![Build Status](https://travis-ci.org/xuchengpeng/emacs.d.svg?branch=master)](https://travis-ci.org/xuchengpeng/emacs.d)
[![](https://tokei.rs/b1/github/xuchengpeng/emacs.d?category=lines)](https://github.com/xuchengpeng/emacs.d)
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

## Documents

http://xuchengpeng.com/

## Install

```sh
$ git clone https://github.com/xuchengpeng/emacs.d.git ~/.emacs.d
```

## Customization

Create a `*.el` file in `~/.emacs.d/personal/preload`, and change the configurations, then restart emacs.

For example:
```el
(setq dotemacs-full-name "user name")           ; User full name
(setq dotemacs-mail-address "user@email.com")   ; Email address
(setq dotemacs-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china, tuna or custom
(setq dotemacs-color-theme 'dotemacs-one)       ; Color theme: default, dark, light or dotemacs-themes
(setq dotemacs-company-enable-yas t)            ; Enable/disable yasnippet for company: t or nil
(setq dotemacs-benchmark-enabled t)             ; Enable/disable initialization benchmark: t or nil
```

If `dotemacs-package-archives` is set to `custom`, you need to set `dotemacs-custom-package-archives`.
```el
(setq dotemacs-package-archives         'custom     ; Package repo: melpa, emacs-china, tuna or custom
      dotemacs-custom-package-archives  '(("gnu"   . "D:/Software/emacs/elpa-mirror/gnu/")
                                          ("melpa" . "D:/Software/emacs/elpa-mirror/melpa/")
                                          ("org"   . "D:/Software/emacs/elpa-mirror/org/")))
```

You and can use [emacs-china](https://elpa.emacs-china.org/) or [tuna](https://mirror.tuna.tsinghua.edu.cn/help/elpa/), or clone it from [elpa-mirror](https://github.com/xuchengpeng/elpa-mirror) or [d12frosted/elpa-mirror](https://github.com/d12frosted/elpa-mirror) to local disk.

## Personalizing

To add your own configurations,  create `*.el` files in `~/.emacs.d/personal`. Sometimes you might want to load code before dotemacs has started loading, create `*.el` files in `~/.emacs.d/personal/preload`.

## Install fonts(Optional)

Install [DejaVu Sans Mono](https://dejavu-fonts.github.io/) or [Source Code Pro](https://github.com/adobe-fonts/source-code-pro).

## Supported Emacs versions

The config should run on Emacs 25.2 or greater and is designed to degrade smoothly - see the [Travis build](https://travis-ci.org/xuchengpeng/emacs.d).
