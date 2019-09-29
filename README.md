# Chuck's Emacs Configuration

[![Made with dotemacs](https://img.shields.io/github/tag/xuchengpeng/.emacs.d.svg?label=release&style=for-the-badge&color=orange)](https://github.com/xuchengpeng/.emacs.d/releases)
[![Made for Emacs 25.2+](https://img.shields.io/badge/Made_for-Emacs_25.2+-blueviolet.svg?style=for-the-badge)](https://www.gnu.org/software/emacs/)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=for-the-badge)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/xuchengpeng/.emacs.d.svg?branch=master)](https://travis-ci.org/xuchengpeng/.emacs.d)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Chuck's Emacs Configuration](#chucks-emacs-configuration)
    - [Install](#install)
    - [Customization](#customization)
    - [Install fonts (Optional)](#install-fonts-optional)
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
```

To add your own private module, customize `dotemacs-private-dir` first, then add your own `packages.el`, `init.el`, `config.el`, `autoload.el` or `autoload/*.el` under this directory, Emacs will load this module automatically on startup, you may need to execute `dotemacs/generate-autoload-file` to update autoloads.

## Install fonts (Optional)

Install your favorite fonts, or you can find some popular fonts [here](https://github.com/xuchengpeng/fonts).

For example:
```el
(setq dotemacs-font (font-spec :family "Fira Mono" :size 14))
(setq dotemacs-cn-font (font-spec :family "STXihei" :size 16))
```

## Supported Emacs versions

The config should run on Emacs 25.2 or greater and is designed to degrade smoothly - see the [Travis build](https://travis-ci.org/xuchengpeng/.emacs.d).
