
(load (concat (expand-file-name user-emacs-directory) "core/core"))

(dotemacs! :feature
           ;; benchmark
           editor
           project
           syntax-checker
           highlight
           utils
           :ui
           ;; themes
           ;; modeline
           treemacs
           window-select
           :completion
           ;; helm
           ivy
           company
           :tools
           eshell
           magit
           snippets
           :lang
           cc
           ;; data
           ;; javascript
           markdown
           ;; org
           ;; web
           )
