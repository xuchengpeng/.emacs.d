;;; dotemacs-vertico.el --- Vertico

;;; Commentary:
;;
;; Vertico configuration.
;;

;;; Code:

(dotemacs-require-package `(vertico :type git :host github :repo "minad/vertico"
                                    :files ("*.el" "extensions/*.el")))
(dotemacs-require-packages '(orderless consult embark embark-consult))

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
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator "[ &]"))

(use-package consult
  :defer t
  :init
  (global-set-key (kbd "C-r") 'consult-line)
  (global-set-key (kbd "C-s") 'consult-line)
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
  :config
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))

(use-package embark
  :defer t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (global-set-key [remap describe-bindings] 'embark-bindings))

(provide 'dotemacs-vertico)
;;; dotemacs-vertico.el ends here