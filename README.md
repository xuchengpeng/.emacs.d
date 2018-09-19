# Chuck's Emacs Configuration

[![Build Status](https://travis-ci.org/xuchengpeng/.emacs.d.svg?branch=master)](https://travis-ci.org/xuchengpeng/.emacs.d)
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Chuck's Emacs Configuration](#chucks-emacs-configuration)
    - [Install](#install)
    - [Customization](#customization)
    - [Install fonts(Optional)](#install-fontsoptional)
    - [Supported Emacs versions](#supported-emacs-versions)

<!-- markdown-toc end -->

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
(setq dotemacs-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china, netease, tuna or custom
(setq dotemacs-theme 'dotemacs-one)             ; Color theme: dotemacs-one, dotemacs-one-light...
(setq dotemacs-company-enable-yas t)            ; Enable/disable yasnippet for company: t or nil
```

If `dotemacs-package-archives` is set to `custom`, you need to set `package-archives`.
```el
(setq dotemacs-package-archives 'custom)
(setq package-archives '(("gnu"   . "/home/user/elpa-mirror/gnu/")
                         ("melpa" . "/home/user/elpa-mirror/melpa/")
                         ("org"   . "/home/user/elpa-mirror/org/")))
```

Customize it with melpa, [emacs-china](https://elpa.emacs-china.org/), [netease](http://mirrors.163.com/elpa/) or [tuna](https://mirror.tuna.tsinghua.edu.cn/help/elpa/), or clone it from [elpa-mirror](https://github.com/xuchengpeng/elpa-mirror) to local disk.

To add your own private module, customize `dotemacs-private-dir` first, then add your own `packages.el`, `init.el`, `config.el`, `autoload.el` or `autoload/*.el` under this directory, Emacs will load this module automatically on startup, you may need to execute `dotemacs/generate-autoload-file` to update autoloads.

## Install fonts(Optional)

Install your favorite fonts, or you can find some popular fonts [here](https://github.com/xuchengpeng/fonts).

For example:
```el
(setq dotemacs-font "Fira Mono")    ; default font
(setq dotemacs-cn-font "STXihei")   ; chinese font
(setq dotemacs-font-size 11)        ; default font size
(setq dotemacs-cn-font-size 16)     ; chinese font size
```

## Supported Emacs versions

The config should run on Emacs 25.2 or greater and is designed to degrade smoothly - see the [Travis build](https://travis-ci.org/xuchengpeng/.emacs.d).
