;;; dotemacs-company.el --- company-mode.

(dotemacs-require-packages '(company))

(defvar +company-backend-alist
  '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

(defun +company--backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

(defun +company-init-backends ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (setq-local company-backends (+company--backends))))

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

  (add-hook 'after-change-major-mode-hook #'+company-init-backends 'append))

(provide 'dotemacs-company)
;;; dotemacs-company.el ends here