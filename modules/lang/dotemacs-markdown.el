;;; dotemacs-markdown.el --- markdown-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(dotemacs-require-package 'markdown-mode)

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

(defun dotemacs-markdown-toc ()
  "Extract headings from the current Markdown buffer.
   The generated and indented TOC will be inserted at point."
  (interactive)
  (let (toc-list markdown-toc)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(#+\\)\\s-+\\(.*\\)" nil t)
        (let* ((level (length (match-string 1)))
               (heading-text (match-string 2))
               (heading-id (downcase (replace-regexp-in-string "[[:space:]]+" "-" heading-text))))
          ;; Remove commas, parentheses from the heading-id
          (setq heading-id (replace-regexp-in-string "[,()]" "" heading-id))
          (push (cons level (cons heading-text heading-id)) toc-list))))
    (setq toc-list (reverse toc-list))
    (dolist (item toc-list)
      (let* ((level (car item))
             (heading-text (cadr item))
             (heading-id (cddr item))
             (indentation (make-string (* 2 (1- level)) ?\ ))
             (line (format "- [%s](#%s)\n" heading-text heading-id)))
        (setq markdown-toc (concat markdown-toc (concat indentation line)))))
    (insert markdown-toc)))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (add-to-list 'dotemacs-major-mode-map-alist '(markdown-mode . markdown-mode-command-map))
  :config
  (setq markdown-command #'dotemacs-markdown-compile)
  (define-key markdown-mode-command-map "i" markdown-mode-style-map))

(provide 'dotemacs-markdown)
;;; dotemacs-markdown.el ends here
