;;; emacs/keybinds/+emacs-bindings.el -*- lexical-binding: t; -*-

(map!
  "/"       '(:ignore t :which-key "search")
  "b"       '(:ignore t :which-key "buffer")
  "c"       '(:ignore t :which-key "code")
  "f"       '(:ignore t :which-key "file")
  "g"       '(:ignore t :which-key "git")
  "h"       '(:ignore t :which-key "help")
  "i"       '(:ignore t :which-key "insert")
  "o"       '(:ignore t :which-key "open")
  "p"       '(:ignore t :which-key "project")
  "q"       '(:ignore t :which-key "quit")
  "s"       '(:ignore t :which-key "snippets")
  "t"       '(:ignore t :which-key "toggle")
  
  ;; search
  "//"      (cond ((featurep! :completion helm) '(helm-occur :which-key "Search buffer"))
                  ((featurep! :completion ivy) '(swiper :which-key "Search buffer")))
  "/b"      (cond ((featurep! :completion helm) '(helm-occur :which-key "Search buffer"))
                  ((featurep! :completion ivy) '(swiper :which-key "Search buffer")))
  "/d"      '(+default/search-cwd :which-key "Search current directory")
  "/D"      '(+default/search-other-cwd :which-key "Search other directory")
  "/f"      '(locate :which-key "Locate file")
  "/i"      '(imenu :which-key "Jump to symbol")
  "/l"      '(ace-link :which-key "Jump to link")
  "/p"      '(+default/search-project :which-key "Search project")
  "/P"      '(+default/search-other-project :which-key "Search other project")
  
  ;; buffer
  "bb"      '(switch-to-buffer :which-key "Switch buffer")
  "bk"      '(kill-this-buffer :which-key "Kill buffer")
  "bn"      '(next-buffer :which-key "Next buffer")
  "bo"      '(dotemacs/kill-other-buffers :which-key "Kill other buffers")
  "bp"      '(previous-buffer :which-key "Previous buffer")
  "br"      '(revert-buffer :which-key "Revert buffer")
  "bR"      '(revert-buffer-with-coding-system :which-key "Reload encoding")
  "bs"      '(save-buffer :which-key "Save buffer")
  "bS"      '(save-some-buffers :which-key "Save all buffers")
  "bz"      '(bury-buffer :which-key "Bury buffer")
  
  ;; code
  "cc"      '(compile :which-key "Compile")
  "cC"      '(recompile :which-key "Recompile")
  "cf"      '(hydra-flycheck/body :which-key "Flycheck")
  "cF"      '(format-all-buffer :which-key "Format")
  "cg"      '(hydra-avy/body :which-key "Goto")
  "cm"      '(hydra-mark/body :which-key "Mark")
  "cM"      '(hydra-multiple-cursors/body :which-key "Multiple cursors")
  "cw"      '(delete-trailing-whitespace :which-key "Delete trailing whitespace")
  
  ;; file
  "fa"      '(projectile-find-other-file :which-key "Find other file")
  "fd"      '(dired :which-key "Find directory")
  "ff"      '(find-file :which-key "Find file")
  "fr"      '(recentf-open-files :which-key "Recent files")
  "fR"      '(projectile-recentf :which-key "Recent project files")
  "fs"      '(save-buffer :which-key "Save file")
  "fS"      '(write-file :which-key "Save file as...")
  "fp"      '(projectile-find-file :which-key "Find file in project")
  
  ;; git
  "gb"      '(magit-blame-addition :which-key "Magit blame")
  "gc"      '(magit-commit :which-key "Magit commit")
  "gd"      '(magit-dispatch-popup :which-key "Magit dispatch")
  "gf"      '(magit-find-file :which-key "Magit find file")
  "gx"      '(magit-file-delete :which-key "Magit file delete")
  "gh"      '(magithub-dispatch-popup :which-key "Magithub dispatch")
  "gi"      '(magit-init :which-key "Initialize repo")
  "gl"      '(magit-log-buffer-file :which-key "Magit buffer log")
  "gL"      '(magit-list-repositories :which-key "List repositories")
  "gs"      '(magit-status :which-key "Magit status")
  "gS"      '(magit-stage-file :which-key "Git stage file")
  "gU"      '(magit-unstage-file :which-key "Git unstage file")
  "gp"      '(magit-push-file :which-key "Magit push popup")
  "gP"      '(magit-pull-popup :which-key "Magit pull popup")
  
  ;; help
  "hF"      '(describe-face :which-key "Describe face")
  "hM"      '(describe-mode :which-key "Describe mode")
  "hc"      '(describe-char :which-key "Describe char")
  "hf"      '(describe-function :which-key "Describe function")
  "hi"      '(info-lookup-symbol :which-key "Info")
  "hk"      '(describe-key :which-key "Describe key")
  "hl"      '(find-library :which-key "Find library")
  "hv"      '(describe-variable :which-key "Describe variable")
  
  ;; insert
  "iy"      '(yank-pop :which-key "Insert from clipboard")
  "is"      '(yas-insert-snippet :which-key "Insert snippet")
  
  ;; open
  "ob"      '(browse-url-of-file :which-key "Default browser")
  "od"      '(dired-jump :which-key "Dired")
  "oe"      '(eshell :which-key "Eshell")
  "oE"      '(shell-pop :which-key "Eshell pop")
  "oo"      '(:ignore t :which-key "Org")
  "ooa"     '(org-agenda :which-key "Org agenda")
  "ooc"     '(org-capture :which-key "Org capture")
  "oop"     '(org-publish-project :which-key "Publish project")
  "ow"      '(hydra-window/body :which-key "Window")
  "oW"      '(hydra-workspaces/body :which-key "Workspaces")
  
  ;; project
  "p."      '(+default/browse-project :which-key "Browse project")
  "p>"      '(dotemacs/browse-in-other-project :which-key "Browse other project")
  "p/"      '(projectile-find-file :which-key "Find file in project")
  "p?"      '(dotemacs/find-file-in-other-project :which-key "Find file in project")
  "pc"      '(projectile-compile-project :which-key "Compile project")
  "pf"      '(projectile-find-file :which-key "Find file in project")
  "pi"      '(projectile-invalidate-cache :which-key "Invalidate cache")
  "po"      '(projectile-find-other-file :which-key "Find other file")
  "pp"      '(projectile-switch-project :which-key "Switch project")
  "pr"      '(projectile-recentf :which-key "Recent project files")
  "ps"      '(+default/search-project :which-key "Search project")
  
  ;; quit
  "qq"      '(kill-emacs :which-key "Quit Emacs")
  "qQ"      '(save-buffers-kill-emacs :which-key "Save and quit Emacs")
  "qr"      '(+workspace/restart-emacs-then-restore :which-key "Restart & restore Emacs")
  "qR"      '(restart-emacs :which-key "Restart Emacs")
  
  ;; snippets
  "s/"      '(yas-visit-snippet-file :which-key "Jump to mode snippet")
  "sn"      '(yas-new-snippet :which-key "New snippet")
  "si"      '(yas-insert-snippet :which-key "Insert snippet")
  "sr"      '(yas-reload-all :which-key "Reload snippets")
  
  ;; toggle
  "ta"      '(auto-fill-mode :which-key "Auto-Fill")
  "tf"      '(flycheck-mode :which-key "Flycheck")
  "tF"      '(toggle-frame-fullscreen :which-key "Frame fullscreen")
  "ti"      '(highlight-indentation-mode :which-key "Indent guides")
  "tI"      '(highlight-indentation-current-column-mode :which-key "Indent guides (column)")
  "tl"      '(display-line-numbers-mode :which-key "Line numbers")
  "to"      '(org-mode :which-key "Org")
  "ts"      '(smartparens-mode :which-key "Smartparens")
  "tt"      (cond ((featurep! :ui neotree) '(neotree-toggle :which-key "NeoTree"))
                  ((featurep! :ui treemacs) '(treemacs :which-key "Treemacs")))
  "tu"      '(undo-tree-mode :which-key "Undo-Tree"))

(define-key!
  "M-x"             'execute-extended-command
  "M-y"             'yank-pop
  "C-x C-f"         'find-file
  "C-x r b"         'bookmark-jump
  "C-s"             (cond ((featurep! :completion helm) '(helm-occur :which-key "Search buffer"))
                          ((featurep! :completion ivy) '(swiper :which-key "Search buffer")))
  "C-r"             (cond ((featurep! :completion helm) '(helm-occur :which-key "Search buffer"))
                          ((featurep! :completion ivy) '(swiper :which-key "Search buffer"))))
