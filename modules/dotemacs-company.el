;;; dotemacs-company.el --- company-mode. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Company configuration.
;;

;;; Code:

(dotemacs-require-packages '(company))

(defvar dotemacs-company-backend-alist
  '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends.
The backends for any mode is built from this.")

(defun dotemacs-set-company-backends ()
  "Set `company-backends' for the current buffer."
  (catch 'found
    (dolist (x dotemacs-company-backend-alist)
      (when (derived-mode-p (car x))
        (setq-local company-backends (cdr x))
        (throw 'found x)))))

(use-package company
  :commands (company-complete-common company-complete-common-or-cycle company-manual-begin company-grab-line)
  :hook ((pre-command . global-company-mode)
         (find-file . global-company-mode))
  :config
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode circe-mode message-mode help-mode gud-mode vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)
        company-backends '(company-capf)
        company-dabbrev-other-buffers nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)

  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") (lambda ()
                                                     (interactive)
                                                     (company-complete-common-or-cycle -1)))
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous)

  (add-hook 'after-change-major-mode-hook #'dotemacs-set-company-backends 'append))

(provide 'dotemacs-company)
;;; dotemacs-company.el ends here
