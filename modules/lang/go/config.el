;;; lang/go/config.el -*- lexical-binding: t; -*-

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (map-local! go-mode-map
    "b"  '(:ignore t       :which-key "build")
    "br" '(+go/run         :which-key "go run .")
    "bb" '(+go/build       :which-key "go build")
    "bc" '(+go/clean       :which-key "go clean")
    "f"  '(gofmt           :which-key "gofmt")
    "i"  '(go-goto-imports :which-key "go-goto-imports")
    "t"  '(:ignore t       :which-key "test")
    "tt" '(+go/test-rerun  :which-key "test rerun")
    "ta" '(+go/test-all    :which-key "test all")
    "ts" '(+go/test-single :which-key "test single")
    "tn" '(+go/test-nested :which-key "test nested")))

(when (modulep! :completion company)
  (use-package company-go
    :after go-mode
    :config
    (set-company-backend! 'go-mode 'company-go)
    (setq company-go-show-annotation t)))
