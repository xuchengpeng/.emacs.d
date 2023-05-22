;;; dotemacs-markdown.el --- markdown-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-packages '(markdown-mode markdown-toc))

(defun dotemacs-markdown-compile-marked (beg end output-buffer)
  "Compiles markdown with the marked program, if available.
Returns its exit code."
  (when (executable-find "marked")
    (apply #'call-process-region
           beg end "marked" nil output-buffer nil
           (when (eq major-mode 'gfm-mode)
             (list "--gfm" "--tables" "--breaks")))))

(defun dotemacs-markdown-compile-pandoc (beg end output-buffer)
  "Compiles markdown with the pandoc program, if available.
Returns its exit code."
  (when (executable-find "pandoc")
    (call-process-region beg end "pandoc" nil output-buffer nil
                         "-f" "markdown"
                         "-t" "html"
                         "--mathjax"
                         "--highlight-style=pygments")))

(defvar dotemacs-markdown-compile-functions
  '(dotemacs-markdown-compile-marked
    dotemacs-markdown-compile-pandoc)
  "A list of markdown compile commands.")

(defun dotemacs-markdown-compile (beg end output-buffer)
  "Compile markdown into html."
  (or (run-hook-with-args-until-success 'dotemacs-markdown-compile-functions
                                        beg end output-buffer)
      (user-error "No markdown program could be found. Install marked, pandoc, markdown or multimarkdown.")))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (add-to-list 'dotemacs-localleader-mode-map-alist '(markdown-mode . markdown-mode-command-map))
  :config
  (setq markdown-command #'dotemacs-markdown-compile)
  (define-key markdown-mode-command-map "i" markdown-mode-style-map))

(provide 'dotemacs-markdown)
;;; dotemacs-markdown.el ends here
