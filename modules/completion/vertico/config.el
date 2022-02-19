;;; completion/vertico/config.el

(use-package vertico
  :hook (dotemacs-first-input . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :after (vertico)
  :config
  (setq completion-styles '(orderless)
        orderless-component-separator "[ &]"))

(use-package consult
  :defer t
  :init
  (define-key!
    [remap apropos]                       #'consult-apropos
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  (setq consult-project-root-function #'dotemacs-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t))

(use-package embark
  :defer t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (define-key! [remap describe-bindings] #'embark-bindings))

(use-package embark-consult
  :after(embark consult)
  :hook(embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :after (vertico)
  :init
  (map-local!
    :keymaps 'minibuffer-local-map
    "M-A" #'marginalia-cycle)
  :config
  (marginalia-mode))
