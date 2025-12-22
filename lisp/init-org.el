;;; init-org.el --- org-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom dotemacs-org-dir "~/org/"
  "Org directory."
  :type 'string)

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
        org-tags-column 0)

  (add-hook
   'org-agenda-mode-hook
   (lambda ()
     (setq-local +modeline-left '(+modeline--buffer-name)
                 +modeline-right '(+modeline--major-mode))))

  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-capture-templates
        '(("t" "Todo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %?\n%i\n%a")
          ("n" "Notes" entry
           (file+headline "notes.org" "Notes")
           "* %u %?\n%i\n%a")
          ("j" "Journal" entry
           (file+olp+datetree "journal.org")
           "* %U %?\n%i\n%a")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("STARTED" . (:inherit (bold font-lock-constant-face)))
          ("HOLD" . (:inherit (bold warning)))
          ("WAIT" . (:inherit (bold warning)))
          ("CANCELLED" . (:inherit (bold error))))
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (python . t)
     (js . t)
     (css . t)
     (sass . t)
     (shell . t)
     (plantuml . t))))

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
  (setq org-publish-timestamp-directory (expand-file-name "org-timestamps/" dotemacs-cache-dir)
        org-export-in-background t
        org-export-headline-levels 4
        org-export-with-section-numbers nil
        org-export-with-author nil
        org-export-with-priority t
        org-export-with-toc t
        org-export-time-stamp-file nil
        org-export-use-babel nil
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

  (defun +org-html-stable-ids-extract-id (datum)
    "Extract a reference from a DATUM.

Return DATUM's `:CUSTOM_ID` if set, or generate a reference from its
`:raw-value` property.  If the DATUM does not have either, return
nil."
    (or
     (org-element-property :CUSTOM_ID datum)
     (let ((value (org-element-property :raw-value datum)))
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

  (defun +org-html-stable-ids-enable ()
    "Enable html stable ids."
    (interactive)
    (advice-add #'org-export-get-reference :override #'+org-export-stable-ids-get-reference)
    (advice-add #'org-html--reference :override #'+org-html-stable-ids-reference))

  (defun +org-html-stable-ids-disable ()
    "Disable html stable ids."
    (interactive)
    (advice-remove #'org-export-get-reference #'+org-export-stable-ids-get-reference)
    (advice-remove #'org-html--reference #'+org-html-stable-ids-reference)))

(provide 'init-org)
;;; init-org.el ends here
