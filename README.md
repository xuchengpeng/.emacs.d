# Chuck's Emacs Configuration

[![Build Status](https://travis-ci.org/xuchengpeng/.emacs.d.svg?branch=master)](https://travis-ci.org/xuchengpeng/.emacs.d)
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

## Documents

http://xuchengpeng.com/

## Install

```sh
$ git clone https://github.com/xuchengpeng/.emacs.d.git ~/.emacs.d
$ cd ~/.emacs.d
$ cp init.example.el init.el
```

## Customization

Change the configurations at the beginning of your `custom.el` or `init.el`, then restart emacs.

For example:
```el
(setq dotemacs-full-name "user name")           ; User full name
(setq dotemacs-mail-address "user@email.com")   ; Email address
(setq dotemacs-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china, tuna or custom
(setq dotemacs-color-theme 'dotemacs-one)       ; Color theme: default, dark, light or dotemacs-themes
(setq dotemacs-company-enable-yas t)            ; Enable/disable yasnippet for company: t or nil
```

If `dotemacs-package-archives` is set to `custom`, you need to set `package-archives`.
```el
(setq dotemacs-package-archives 'custom     ; Package repo: melpa, emacs-china, tuna or custom
      package-archives          '(("gnu"   . "/home/user/elpa-mirror/gnu/")
                                  ("melpa" . "/home/user/elpa-mirror/melpa/")
                                  ("org"   . "/home/user/elpa-mirror/org/")))
```

Customize it with melpa, [emacs-china](https://elpa.emacs-china.org/) or [tuna](https://mirror.tuna.tsinghua.edu.cn/help/elpa/), or clone it from [elpa-mirror](https://github.com/xuchengpeng/elpa-mirror) to local disk.

## Install fonts(Optional)

Install [DejaVu Sans Mono](https://dejavu-fonts.github.io/) or [Source Code Pro](https://github.com/adobe-fonts/source-code-pro).

## Supported Emacs versions

The config should run on Emacs 25.2 or greater and is designed to degrade smoothly - see the [Travis build](https://travis-ci.org/xuchengpeng/.emacs.d).
