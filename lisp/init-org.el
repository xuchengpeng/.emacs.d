;;; init-org.el --- org-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom dotemacs-org-dir "~/org/"
  "Org directory."
  :type 'string)

(defcustom dotemacs-ob-load-languages
  '((C . t)
    (emacs-lisp . t)
    (python . t)
    (js . t)
    (css . t)
    (sass . t)
    (shell . t)
    (plantuml . t))
  "Alist of org babel languages."
  :type 'alist)

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (keymap-global-set "C-c a" 'org-agenda)
  (keymap-global-set "C-c c" 'org-capture)
  :config
  (setq org-directory dotemacs-org-dir
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-agenda-files (list org-directory)
        org-agenda-window-setup 'current-window
        org-persist-directory (expand-file-name "org-persist" dotemacs-cache-dir)
        org-startup-indented t
        org-log-into-drawer t
        org-tags-column 0
        org-confirm-babel-evaluate nil
        org-agenda-tags-column 0
        org-agenda-custom-commands
        '(("d" "Daily Agenda"
           ((tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "Important tasks")))
            (agenda "" ((org-agenda-overriding-header "Today")
                        (org-agenda-span 1)
                        (org-deadline-warning-days 0)
                        (org-scheduled-past-days 0)))
            (agenda "" ((org-agenda-overriding-header "Next 3 days")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+1d")
                        (org-agenda-span 3)
                        (org-deadline-warning-days 0)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            (agenda "" ((org-agenda-overriding-header "Upcoming 14 days")
                        (org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))))))

  (add-hook
   'org-agenda-mode-hook
   (lambda ()
     (setq-local +modeline-left '(+modeline--buffer-name)
                 +modeline-right '(+modeline--major-mode))))

  (setq org-default-notes-file (expand-file-name "notes.org" dotemacs-local-dir)
        org-capture-templates
        '(("t" "Todo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %^{Title}%?\nDEADLINE: %^T\n")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("STARTED" . font-lock-constant-face)
          ("HOLD" . warning)
          ("WAIT" . warning)
          ("CANCELLED" . error))
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   dotemacs-ob-load-languages))

(use-package org-clock
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" dotemacs-cache-dir))
  :config
  (setq org-clock-persist 'history
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-history-length 20)
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package htmlize
  :ensure t)

(use-package ox-publish
  :commands org-publish
  :config
  (require 'ox-html)
  (setq org-publish-timestamp-directory (expand-file-name "org-timestamps/" dotemacs-cache-dir)
        org-export-in-background t
        org-export-headline-levels 4
        org-export-with-section-numbers nil
        org-export-with-author nil
        org-export-with-priority t
        org-export-with-toc t
        org-export-time-stamp-file nil
        org-export-use-babel t
        org-html-checkbox-type 'html
        org-html-container-element "section"
        org-html-divs '((preamble  "header" "preamble")
                        (content   "main" "content")
                        (postamble "footer" "postamble"))
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-validation-link nil)

  (defun +org-html-stable-ids-extract-value (datum)
    "Extract raw-value from a DATUM."
    (let ((value (or (org-element-property :raw-value datum)
                     (org-element-property-raw :value datum))))
      (unless (org-element-deferred-p value)
        value)))

  (defun +org-html-stable-ids-extract-id (datum)
    "Extract a reference from a DATUM.

Return DATUM's `:CUSTOM_ID` if set, or generate a reference from its
`:raw-value` property.  If the DATUM does not have either, return
nil."
    (or
     (org-element-property :CUSTOM_ID datum)
     (let ((value (+org-html-stable-ids-extract-value datum)))
       (when value
         (+org-html-stable-ids-to-kebab-case value)))))

  (defun +org-html-stable-ids-to-kebab-case (string)
    "Convert STRING to kebab-case."
    (downcase
     (string-trim
      (replace-regexp-in-string "[^A-Za-z0-9\u4e00-\u9fa5]+" "-" string)
      "-" "-")))

  (defun +org-export-stable-ids-get-reference (datum info)
    "Return a reference for DATUM with INFO.

    Raise an error if the ID was used in the document before."
    (let ((cache (plist-get info :internal-references))
	      (id (+org-html-stable-ids-extract-id datum)))
	  (or (car (rassq datum cache))
	      (if (assoc id cache)
		      (user-error "Duplicate ID: %s" id)
	        (when id
		      (push (cons id datum) cache)
		      (plist-put info :internal-references cache)
		      id)))))

  (defun +org-html-stable-ids-reference (datum info &optional named-only)
    "Call `org-export-get-reference` to get a reference for DATUM with INFO.

If `NAMED-ONLY` is non-nil, return nil."
    (unless named-only
      (org-export-get-reference datum info)))

  (advice-add #'org-export-get-reference :override #'+org-export-stable-ids-get-reference)
  (advice-add #'org-html--reference :override #'+org-html-stable-ids-reference)

  (org-export-define-derived-backend '+org-html 'html
    :translate-alist '((template . +org-html-template))
    :menu-entry '(?h "Export to HTML"
                     ((?b "+org-html to buffer" +org-html-export-as-html)
                      (?f "+org-html to file" +org-html-export-to-html))))

  (defun +org-html-template (contents info)
    "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (concat
     "<!DOCTYPE html>\n"
     "<html lang=\"en\" class=\"scroll-smooth\">\n"
     "<head>\n"
     "<meta charset=\"utf-8\">\n"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
     (format "<title>%s</title>\n" (org-export-data (plist-get info :title) info))
     "<link rel=\"stylesheet\" href=\"https://orgmode.org/worg/style/worg.css\">\n"
     "<style> body { max-width: 72rem; width: 100%; margin: 0 auto; padding: 1em; } </style>\n"
     (when (and (memq (plist-get info :with-latex) '(mathjax t))
                (org-element-map (plist-get info :parse-tree) '(latex-fragment latex-environment) #'identity info t nil t))
       "<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js\"></script>\n")
     "</head>\n"
     "<body>\n"
     "<header>\n"
     (format "<h1>%s</h1>\n" (org-export-data (plist-get info :title) info))
     "</header>\n"
     "<main>\n"
     contents
     "</main>\n"
     "<footer>\n"
     "<p>Powered by <a href=\"https://www.gnu.org/software/emacs/\">Emacs</a></p>\n"
     "</footer>\n"
     "</body>\n"
     "</html>"))

  (defun +org-html-export-as-html (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to a HTML buffer."
    (interactive)
    (org-export-to-buffer '+org-html "*+org-html export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (set-auto-mode t))))

  (defun +org-html-export-to-html (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to a HTML file."
    (let* ((file (org-export-output-file-name ".html" subtreep)))
      (org-export-to-file '+org-html file
        async subtreep visible-only body-only ext-plist))))

(provide 'init-org)
;;; init-org.el ends here
