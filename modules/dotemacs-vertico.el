;;; dotemacs-vertico.el --- Vertico. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Vertico configuration.
;;

;;; Code:

(dotemacs-require-package `(vertico :type git :host github :repo "minad/vertico"
                                    :files ("*.el" "extensions/*.el")))
(dotemacs-require-packages '(orderless consult embark embark-consult marginalia))

(use-package vertico
  :hook (pre-command . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t))
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

(use-package orderless
  :after (vertico)
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space))

(defvar consult-fd-args nil
  "Command line arguments for fd.")

(use-package consult
  :defer t
  :init
  (global-set-key [remap apropos]                       'consult-apropos)
  (global-set-key [remap bookmark-jump]                 'consult-bookmark)
  (global-set-key [remap evil-show-marks]               'consult-mark)
  (global-set-key [remap evil-show-registers]           'consult-register)
  (global-set-key [remap goto-line]                     'consult-goto-line)
  (global-set-key [remap imenu]                         'consult-imenu)
  (global-set-key [remap locate]                        'consult-locate)
  (global-set-key [remap load-theme]                    'consult-theme)
  (global-set-key [remap man]                           'consult-man)
  (global-set-key [remap recentf-open-files]            'consult-recent-file)
  (global-set-key [remap switch-to-buffer]              'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame]  'consult-buffer-other-frame)
  (global-set-key [remap yank-pop]                      'consult-yank-pop)
  (unless consult-fd-args
    (setq consult-fd-args (concat "fd --color=never -i -H -E .git --regex"
                            (if IS-WINDOWS " --path-separator=/" ""))))
  :config
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-find-args (if IS-WINDOWS
                              "find . -not ( -wholename \\*/.\\* -prune )"
                            consult-find-args)))

;;;###autoload
(defun consult--fd-make-builder ()
  "Consult fd command builder."
  (let ((cmd (split-string-and-unquote consult-fd-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended t)))
        (when re
          (cons (append cmd
                        (list (consult--join-regexps re 'extended))
                        opts)
                hl))))))

(autoload #'consult--directory-prompt "consult")
;;;###autoload
(defun consult-fd (&optional dir initial)
  "Search for files in DIR matching input regexp given INITIAL input."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                (default-directory dir)
                (builder (consult--fd-make-builder paths)))
    (find-file (consult--find prompt builder initial))))

(defun dotemacs-find-file (&optional dir initial)
  "Search for files in DIR matching input regexp given INITIAL input."
  (interactive "P")
  (cond
   ((executable-find "fd")
    (consult-fd dir initial))
   ((executable-find "find")
    (consult-find dir initial))
   (find-file dir initial)))

(defun dotemacs-search-symbol-at-point ()
  "Search symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun dotemacs-search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (consult-ripgrep default-directory)))

(defun dotemacs-search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (dotemacs-search-cwd 'other))

(use-package embark
  :defer t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (global-set-key [remap describe-bindings] 'embark-bindings))

(use-package marginalia
  :after (vertico)
  :init
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
  :config
  (marginalia-mode))

(provide 'dotemacs-vertico)
;;; dotemacs-vertico.el ends here
