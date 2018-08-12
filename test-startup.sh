#!/bin/bash
set -ev

emacs -nw -l init.test.el --eval "(add-hook 'dotemacs-post-init-hook #'dotemacs|run-test t)"
