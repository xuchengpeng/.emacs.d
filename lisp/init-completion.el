;;; init-completion.el --- Completion  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-cycle t))

(use-package orderless
  :ensure t
  :after (vertico)
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space))

(use-package consult
  :ensure t
  :after (vertico)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (keymap-global-set "<remap> <apropos>" #'consult-apropos)
  (keymap-global-set "<remap> <bookmark-jump>" #'consult-bookmark)
  (keymap-global-set "<remap> <goto-line>" #'consult-goto-line)
  (keymap-global-set "<remap> <imenu>" #'consult-imenu)
  (keymap-global-set "<remap> <Info-search>" #'consult-info)
  (keymap-global-set "<remap> <locate>" #'consult-locate)
  (keymap-global-set "<remap> <load-theme>" #'consult-theme)
  (keymap-global-set "<remap> <man>" #'consult-man)
  (keymap-global-set "<remap> <recentf-open-files>" #'consult-recent-file)
  (keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer)
  (keymap-global-set "<remap> <switch-to-buffer-other-window>" #'consult-buffer-other-window)
  (keymap-global-set "<remap> <switch-to-buffer-other-frame>"  #'consult-buffer-other-frame)
  (keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
  (keymap-global-set "C-r" #'consult-line)
  (keymap-global-set "C-s" #'consult-line)
  (with-eval-after-load 'xref
    (when (executable-find "rg")
      (setq xref-search-program 'ripgrep))
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))
  :config
  (setq consult-narrow-key "<")
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5))

(defun +find-file (&optional dir initial)
  "Search for files in DIR matching input regexp given INITIAL input."
  (interactive "P")
  (cond
   ((executable-find "fd")
    (consult-fd dir initial))
   ((executable-find "find")
    (consult-find dir initial))
   (find-file dir initial)))

(defun +search-cwd (&optional other)
  "Conduct a text search in files under the current folder.
If prefix OTHER is set, prompt for a directory to search from."
  (interactive "P")
  (let ((dir (if other
                 (read-directory-name "Search directory: ")
               default-directory)))
    (consult-ripgrep dir nil)))

(defun +search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+search-cwd 'other))

(use-package embark
  :ensure t
  :after (vertico)
  :config
  (keymap-global-set "C-." 'embark-act)
  (keymap-global-set "C-;" 'embark-dwim)
  (keymap-global-set "C-h B" 'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command)
  (keymap-set embark-general-map "." #'mark))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :after (vertico)
  :init
  (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle)
  :config
  (marginalia-mode))

(use-package emacs
  :ensure nil
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(use-package dabbrev
  :ensure nil
  :after corfu
  :init
  ;; Swap M-/ and C-M-/
  (keymap-global-set "M-/" 'dabbrev-completion)
  (keymap-global-set "C-M-/" 'dabbrev-expand)
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package cape
  :ensure t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package tempel
  :ensure t
  :commands (tempel-complete tempel-insert)
  :init
  (keymap-global-set "M-+" 'tempel-complete)
  (keymap-global-set "M-*" 'tempel-insert)

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (setq tempel-path (append (ensure-list tempel-path) dotemacs-tempel-path)))

(provide 'init-completion)
;;; init-completion.el ends here
