
(load (concat (expand-file-name user-emacs-directory) "core/core"))

(dotemacs! :feature
           editor
           project
           utils
           :ui
           themes
           modeline
           window-select
           :completion
           ;; helm
           ivy
           company
           :tools
           eshell
           :lang
           cc
           markdown
           )
