#!/bin/bash
set -ev

emacs -l init.test.el --eval "(add-hook 'dotemacs-post-init-hook #'dotemacs|run-test t)"
