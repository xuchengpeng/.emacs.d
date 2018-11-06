;;; dotemacs-themes-common.el -*- lexical-binding: t; -*-

(defvar dotemacs-themes-common-faces
  '(;; --- custom faces -----------------------
    (dotemacs-modeline-error
     :background (dotemacs-darken red 0.25)
     :foreground base0
     :distant-foreground base0)

    ;; --- base faces -------------------------
    (bold        :weight 'bold :foreground (unless bold base8))
    (italic      :slant  'italic)
    (bold-italic :inherit '(bold italic))

    (default :background bg :foreground fg)
    (fringe :inherit 'default :foreground base4)
    (region               :background region     :foreground nil   :distant-foreground (dotemacs-darken fg 0.2))
    (highlight            :background highlight  :foreground base0 :distant-foreground base8)
    (cursor               :background highlight)
    (shadow               :foreground base5)
    (minibuffer-prompt    :foreground highlight)
    (tooltip              :background base3 :foreground fg)
    (secondary-selection  :background grey)
    (lazy-highlight       :background dark-blue  :foreground base8 :distant-foreground base0 :weight 'bold)
    (match                :foreground green      :background base0 :weight 'bold)
    (trailing-whitespace  :background red)
    (nobreak-space        :inherit 'default :underline nil)
    (vertical-border      :background vertical-bar :foreground vertical-bar)
    (link                 :foreground highlight :underline t :weight 'bold)

    (error   :foreground error)
    (warning :foreground warning)
    (success :foreground success)

    (font-lock-builtin-face              :foreground builtin)
    (font-lock-comment-face              :foreground comments)
    (font-lock-comment-delimiter-face    :inherit 'font-lock-comment-face)
    (font-lock-doc-face                  :inherit 'font-lock-comment-face :foreground doc-comments)
    (font-lock-constant-face             :foreground constants)
    (font-lock-function-name-face        :foreground functions)
    (font-lock-keyword-face              :foreground keywords)
    (font-lock-string-face               :foreground strings)
    (font-lock-type-face                 :foreground type)
    (font-lock-variable-name-face        :foreground variables)
    (font-lock-warning-face              :inherit 'warning)
    (font-lock-negation-char-face        :inherit 'bold :foreground operators)
    (font-lock-preprocessor-face         :inherit 'bold :foreground operators)
    (font-lock-preprocessor-char-face    :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-backslash :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-construct :inherit 'bold :foreground operators)

    ;; mode-line / header-line
    (mode-line           :background bg     :foreground fg     :distant-foreground bg)
    (mode-line-inactive  :background bg-alt :foreground fg-alt :distant-foreground bg-alt)
    (mode-line-emphasis  :foreground highlight :distant-foreground bg)
    (mode-line-highlight :inherit 'highlight :distant-foreground bg)
    (mode-line-buffer-id :weight 'bold)
    (header-line :inherit 'mode-line :distant-foreground bg)

    ;; 1. Line number faces must explicitly disable its text style attributes
    ;;    because nearby faces may "bleed" into the line numbers otherwise.
    ;; 2. All other line number plugin faces should &inherit from these.
    (line-number
     :inherit 'default
     :foreground base5 :distant-foreground nil
     :weight 'normal :italic nil :underline nil :strike-through nil)
    (line-number-current-line
     :inherit '(hl-line default)
     :foreground fg :distant-foreground nil
     :weight 'normal :italic nil :underline nil :strike-through nil)


    ;; --- built-in plugin faces --------------
    ;; cperl
    (cperl-array-face          :weight 'bold :inherit 'font-lock-variable-name-face)
    (cperl-hash-face           :weight 'bold :slant 'italic :inherit 'font-lock-variable-name-face)
    (cperl-nonoverridable-face :inherit 'font-lock-builtin-face)

    ;; compilation
    (compilation-column-number  :inherit 'font-lock-comment-face)
    (compilation-line-number    :foreground highlight)
    (compilation-error   :inherit 'error   :weight 'bold)
    (compilation-warning :inherit 'warning :slant 'italic)
    (compilation-info    :inherit 'success)
    (compilation-mode-line-exit :inherit 'compilation-info)
    (compilation-mode-line-fail :inherit 'compilation-error)

    ;; custom
    (custom-button                  :foreground blue   :background bg     :box '(:line-width 1 :style none))
    (custom-button-unraised         :foreground violet :background bg     :box '(:line-width 1 :style none))
    (custom-button-pressed-unraised :foreground bg     :background violet :box '(:line-width 1 :style none))
    (custom-button-pressed          :foreground bg     :background blue   :box '(:line-width 1 :style none))
    (custom-button-mouse            :foreground bg     :background blue   :box '(:line-width 1 :style none))

    (custom-variable-button   :foreground green :underline t)
    (custom-saved             :foreground green :background (dotemacs-blend green bg 0.2) :bold bold)
    (custom-comment           :foreground fg :background region)
    (custom-comment-tag       :foreground grey)
    (custom-modified          :foreground blue :background (dotemacs-blend blue bg 0.2))
    (custom-variable-tag      :foreground magenta)
    (custom-visibility        :foreground blue :underline nil)
    (custom-group-subtitle    :foreground red)
    (custom-group-tag         :foreground violet)
    (custom-group-tag-1       :foreground blue)
    (custom-set               :foreground yellow :background bg)
    (custom-themed            :foreground yellow :background bg)
    (custom-invalid           :foreground red :background (dotemacs-blend red bg 0.2))
    (custom-variable-obsolete :foreground grey :background bg)
    (custom-state             :foreground green :background (dotemacs-blend green bg 0.2))
    (custom-changed           :foreground blue :background bg)

    ;; dired
    (dired-directory  :foreground builtin)
    (dired-ignored    :foreground comments)
    (dired-flagged    :foreground red)
    (dired-header     :foreground blue :weight 'bold)
    (dired-mark       :foreground orange :weight 'bold)
    (dired-marked     :foreground magenta :weight 'bold)
    (dired-perm-write :foreground fg :underline t)
    (dired-symlink    :foreground cyan :weight 'bold)
    (dired-warning    :foreground warning)

    ;; ediff
    (ediff-fine-diff-A    :background (dotemacs-blend selection bg 0.7) :weight 'bold)
    (ediff-fine-diff-B    :inherit 'ediff-fine-diff-A)
    (ediff-fine-diff-C    :inherit 'ediff-fine-diff-A)
    (ediff-current-diff-A :background (dotemacs-blend selection bg 0.3))
    (ediff-current-diff-B :inherit 'ediff-current-diff-A)
    (ediff-current-diff-C :inherit 'ediff-current-diff-A)
    (ediff-even-diff-A    :inherit 'hl-line)
    (ediff-even-diff-B    :inherit 'ediff-even-diff-A)
    (ediff-even-diff-C    :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-A     :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-B     :inherit 'ediff-odd-diff-A)
    (ediff-odd-diff-C     :inherit 'ediff-odd-diff-A)

    ;; elfeed
    (elfeed-log-debug-level-face :foreground comments)
    (elfeed-log-error-level-face :inherit 'error)
    (elfeed-log-info-level-face  :inherit 'success)
    (elfeed-log-warn-level-face  :inherit 'warning)
    (elfeed-search-date-face     :foreground violet)
    (elfeed-search-feed-face     :foreground blue)
    (elfeed-search-tag-face      :foreground comments)
    (elfeed-search-title-face    :foreground comments)
    (elfeed-search-filter-face   :foreground violet)
    (elfeed-search-unread-count-face :foreground yellow)
    (elfeed-search-unread-title-face :foreground fg :weight 'bold)

    ;; eshell
    (eshell-prompt        :foreground highlight :weight 'bold)
    (eshell-ls-archive    :foreground magenta)
    (eshell-ls-backup     :foreground yellow)
    (eshell-ls-clutter    :foreground red)
    (eshell-ls-directory  :foreground blue)
    (eshell-ls-executable :foreground green)
    (eshell-ls-missing    :foreground red)
    (eshell-ls-product    :foreground orange)
    (eshell-ls-readonly   :foreground orange)
    (eshell-ls-special    :foreground violet)
    (eshell-ls-symlink    :foreground cyan)
    (eshell-ls-unreadable :foreground base5)

    ;; flx-ido
    (flx-highlight-face :weight 'bold :foreground yellow :underline nil)
    
    ;; hi-lock
    (hi-yellow   :background yellow)
    (hi-pink     :background magenta)
    (hi-red-b    :foreground red :weight 'bold)
    (hi-green    :background green)
    (hi-green-b  :foreground green :weight 'bold)
    (hi-blue     :background blue)
    (hi-blue-b   :foreground blue :weight 'bold)
    ;; (hi-black-b  :weight 'bold)
    ;; (hi-black-hb :inherit 'variable-pitch :weight 'bold :height 1.67)

    ;; hl-line
    (hl-line :background bg-alt)

    ;; ido
    (ido-first-match :foreground orange)
    (ido-indicator   :foreground red :background bg)
    (ido-only-match  :foreground green)
    (ido-subdir      :foreground violet)
    (ido-virtual     :foreground comments)

    ;; isearch
    (isearch :inherit 'lazy-highlight :weight 'bold)
    (isearch-fail :background error :foreground base0 :weight 'bold)

    ;; linum
    ((linum &inherit line-number))

    ;; message
    (message-header-name       :foreground green)
    (message-header-subject    :foreground highlight :weight 'bold)
    (message-header-to         :foreground highlight :weight 'bold)
    (message-header-cc         :inherit 'message-header-to :foreground (dotemacs-darken highlight 0.15))
    (message-header-other      :foreground violet)
    (message-header-newsgroups :foreground yellow)
    (message-header-xheader    :foreground doc-comments)
    (message-separator         :foreground comments)
    (message-mml               :foreground comments :slant 'italic)
    (message-cited-text        :foreground magenta)

    ;; term
    (term               :foreground fg)
    (term-bold          :weight 'bold)
    (term-color-black   :background base0   :foreground base0)
    (term-color-red     :background red     :foreground red)
    (term-color-green   :background green   :foreground green)
    (term-color-yellow  :background yellow  :foreground yellow)
    (term-color-blue    :background blue    :foreground blue)
    (term-color-magenta :background magenta :foreground magenta)
    (term-color-cyan    :background cyan    :foreground cyan)
    (term-color-white   :background base8   :foreground base8)

    ;; window-divider
    (window-divider :inherit 'vertical-border)
    (window-divider-first-pixel :inherit 'window-divider)
    (window-divider-last-pixel  :inherit 'window-divider)


    ;; --- plugin faces -----------------------
    ;; anzu
    (anzu-replace-highlight :background base0 :foreground red   :weight 'bold :strike-through t)
    (anzu-replace-to        :background base0 :foreground green :weight 'bold)
    
    ;; avy
    (avy-background-face :foreground comments)
    (avy-lead-face :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (avy-lead-face-0
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (dotemacs-lighten highlight 0.3))
     (&light :background (dotemacs-darken highlight 0.3)))
    (avy-lead-face-1
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (dotemacs-lighten highlight 0.6))
     (&light :background (dotemacs-darken highlight 0.6)))
    (avy-lead-face-2
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (dotemacs-lighten highlight 0.9))
     (&light :background (dotemacs-darken highlight 0.9)))

    ;; bookmark+
    (bmkp-*-mark :foreground bg :background yellow)
    (bmkp->-mark :foreground yellow)
    (bmkp-D-mark :foreground bg :background red)
    (bmkp-X-mark :foreground red)
    (bmkp-a-mark :background red)
    (bmkp-bad-bookmark :foreground bg :background yellow)
    (bmkp-bookmark-file :foreground violet :background bg-alt)
    (bmkp-bookmark-list :background bg-alt)
    (bmkp-buffer :foreground blue)
    (bmkp-desktop :foreground bg :background violet)
    (bmkp-file-handler :background red)
    (bmkp-function :foreground green)
    (bmkp-gnus :foreground orange)
    (bmkp-heading :foreground yellow)
    (bmkp-info :foreground cyan)
    (bmkp-light-autonamed :foreground bg-alt :background cyan)
    (bmkp-light-autonamed-region :foreground bg-alt :background red)
    (bmkp-light-fringe-autonamed :foreground bg-alt :background violet)
    (bmkp-light-fringe-non-autonamed :foreground bg-alt :background green)
    (bmkp-light-mark :foreground bg :background cyan)
    (bmkp-light-non-autonamed :foreground bg :background violet)
    (bmkp-light-non-autonamed-region :foreground bg :background red)
    (bmkp-local-directory :foreground bg :background violet)
    (bmkp-local-file-with-region :foreground yellow)
    (bmkp-local-file-without-region :foreground comments)
    (bmkp-man :foreground violet)
    (bmkp-no-jump :foreground comments)
    (bmkp-no-local :foreground yellow)
    (bmkp-non-file :foreground green)
    (bmkp-remote-file :foreground orange)
    (bmkp-sequence :foreground blue)
    (bmkp-su-or-sudo :foreground red)
    (bmkp-t-mark :foreground violet)
    (bmkp-url :foreground blue :underline t)
    (bmkp-variable-list :foreground green)

    ;; calfw
    (cfw:face-title              :foreground blue                     :weight 'bold :height 2.0 :inherit 'variable-pitch)
    (cfw:face-header             :foreground (dotemacs-blend blue bg 0.8) :weight 'bold)
    (cfw:face-sunday             :foreground (dotemacs-blend red bg 0.8)  :weight 'bold)
    (cfw:face-saturday           :foreground (dotemacs-blend red bg 0.8)  :weight 'bold)
    (cfw:face-holiday            :foreground nil :background bg-alt   :weight 'bold)
    (cfw:face-grid               :foreground vertical-bar)
    (cfw:face-periods            :foreground yellow)
    (cfw:face-toolbar            :foreground nil :background nil)
    (cfw:face-toolbar-button-off :foreground base6                    :weight 'bold             :inherit 'variable-pitch)
    (cfw:face-toolbar-button-on  :foreground blue                     :weight 'bold             :inherit 'variable-pitch)
    (cfw:face-default-content    :foreground fg)
    (cfw:face-day-title          :foreground fg                       :weight 'bold)
    (cfw:face-today-title        :foreground bg  :background blue     :weight 'bold)
    (cfw:face-default-day                                             :weight 'bold)
    (cfw:face-today              :foreground nil :background nil      :weight 'bold)
    (cfw:face-annotation         :foreground violet)
    (cfw:face-disable            :foreground grey)
    (cfw:face-select             :background region)

    ;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight :distant-foreground base0 :weight 'bold)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (company-tooltip-search-selection :background (dotemacs-darken selection 0.25))
    (company-tooltip-selection  :background selection :weight 'bold)
    (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
    (company-tooltip-annotation                       :foreground violet :distant-foreground bg)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview                              :foreground comments)
    (company-preview-common     :background base3 :foreground highlight)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)

    ;; company-box
    (company-box-candidate :foreground fg)

    ;; circe
    (circe-fool :foreground doc-comments)
    (circe-highlight-nick-face :weight 'bold :foreground constants)
    (circe-prompt-face :weight 'bold :foreground highlight)
    (circe-server-face :foreground comments)
    (circe-my-message-face :weight 'bold)

    ;; diff-hl
    (diff-hl-change :foreground vc-modified)
    (diff-hl-delete :foreground vc-deleted)
    (diff-hl-insert :foreground vc-added)

    ;; diff-mode
    (diff-added   :inherit 'hl-line :foreground green)
    (diff-changed :foreground violet)
    (diff-context
     (&dark  :foreground (dotemacs-darken fg 0.12))
     (&light :foreground (dotemacs-lighten fg 0.12)))
    (diff-removed :foreground red :background base3)
    (diff-header  :foreground cyan :background nil)
    (diff-file-header :foreground blue :background nil)
    (diff-hunk-header :foreground violet)
    (diff-refine-added   :inherit 'diff-added :inverse-video t)
    (diff-refine-changed :inherit 'diff-changed :inverse-video t)
    (diff-refine-removed :inherit 'diff-removed :inverse-video t)

    ;; dired+
    (diredp-file-name              :foreground base8)
    (diredp-dir-name               :foreground base8 :weight 'bold)
    (diredp-ignored-file-name      :foreground base5)
    (diredp-compressed-file-suffix :foreground base5)
    (diredp-symlink                :foreground violet)
    (diredp-dir-heading            :foreground blue  :weight 'bold)
    (diredp-file-suffix            :foreground violet)
    (diredp-read-priv              :foreground magenta)
    (diredp-write-priv             :foreground green)
    (diredp-exec-priv              :foreground yellow)
    (diredp-rare-priv              :foreground red   :weight 'bold)
    (diredp-dir-priv               :foreground blue  :weight 'bold)
    (diredp-no-priv                :foreground base5)
    (diredp-number                 :foreground magenta)
    (diredp-date-time              :foreground blue)

    ;; dired-k
    (dired-k-directory :foreground blue)

    ;; dired-subtree
    (dired-subtree-depth-1-face :background (dotemacs-darken bg-alt 0.02))
    (dired-subtree-depth-2-face :background (dotemacs-darken bg-alt 0.04))
    (dired-subtree-depth-3-face :background (dotemacs-darken bg-alt 0.06))
    (dired-subtree-depth-4-face :background (dotemacs-darken bg-alt 0.08))
    (dired-subtree-depth-5-face :background (dotemacs-darken bg-alt 0.10))
    (dired-subtree-depth-6-face :background (dotemacs-darken bg-alt 0.12))

    ;; diredfl
    (diredfl-autofile-name          :foreground base4 :background bg-alt)
    (diredfl-compressed-file-name   :foreground yellow :background bg-alt)
    (diredfl-compressed-file-suffix :foreground (dotemacs-blend orange bg-alt 0.6) :background bg-alt)
    (diredfl-date-time              :foreground cyan :background bg-alt :weight 'light)
    (diredfl-deletion               :foreground red :background (dotemacs-blend red bg-alt 0.2) :weight 'bold)
    (diredfl-deletion-file-name     :foreground red :background (dotemacs-blend red bg-alt 0.2))
    (diredfl-dir-heading            :foreground blue :background bg-alt :weight 'bold)
    (diredfl-dir-name               :foreground blue :background bg-alt)
    (diredfl-dir-priv               :foreground blue :background bg-alt)
    (diredfl-exec-priv              :foreground green :background bg-alt)
    (diredfl-executable-tag         :foreground green :background bg-alt)
    (diredfl-file-name              :foreground fg :background bg-alt)
    (diredfl-file-suffix            :foreground (dotemacs-blend fg bg-alt 0.6) :background bg-alt)
    (diredfl-flag-mark              :foreground yellow :background (dotemacs-blend yellow bg-alt 0.2) :weight 'bold)
    (diredfl-flag-mark-line         :background (dotemacs-blend yellow bg-alt 0.1))
    (diredfl-ignored-file-name      :foreground comments :background bg-alt)
    (diredfl-link-priv              :foreground violet :background bg-alt)
    (diredfl-no-priv                :foreground fg :background bg-alt)
    (diredfl-number                 :foreground orange :background bg-alt)
    (diredfl-other-priv             :foreground magenta :background bg-alt)
    (diredfl-rare-priv              :foreground fg :background bg-alt)
    (diredfl-read-priv              :foreground yellow :background bg-alt)
    (diredfl-symlink                :foreground violet :background bg-alt)
    (diredfl-tagged-autofile-name   :foreground base5 :background bg-alt)
    (diredfl-write-priv             :foreground red :background bg-alt)

    ;; elscreen
    (elscreen-tab-background-face     :background bg)
    (elscreen-tab-control-face        :background bg     :foreground bg)
    (elscreen-tab-current-screen-face :background bg-alt :foreground fg)
    (elscreen-tab-other-screen-face   :background bg     :foreground fg-alt)

    ;; erc
    (erc-button :weight 'bold :underline t)
    (erc-default-face :inherit 'default)
    (erc-action-face  :weight 'bold)
    (erc-command-indicator-face :weight 'bold)
    (erc-direct-msg-face :foreground magenta)
    (erc-error-face :inherit 'error)
    (erc-header-line :background (dotemacs-darken bg-alt 0.15) :foreground highlight)
    (erc-input-face :foreground green)
    (erc-current-nick-face :foreground green :weight 'bold)
    (erc-timestamp-face :foreground blue :weight 'bold)
    (erc-nick-default-face :weight 'bold)
    (erc-nick-msg-face :foreground magenta)
    (erc-nick-prefix-face :inherit 'erc-nick-default-face)
    (erc-my-nick-face :foreground green :weight 'bold)
    (erc-my-nick-prefix-face :inherit 'erc-my-nick-face)
    (erc-notice-face :foreground comments)
    (erc-prompt-face :foreground highlight :weight 'bold)

    ;; evil
    (evil-ex-info                   :foreground error :slant 'italic)
    (evil-ex-search                 :background highlight :foreground base0 :weight 'bold)
    (evil-ex-substitute-matches     :background base0 :foreground red   :strike-through t :weight 'bold)
    (evil-ex-substitute-replacement :background base0 :foreground green :weight 'bold)
    (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight)

    ;; evil-mc
    (evil-mc-cursor-default-face :background magenta :foreground base0 :inverse-video nil)
    (evil-mc-region-face :inherit 'region)
    (evil-mc-cursor-bar-face :height 1 :background magenta :foreground base0)
    (evil-mc-cursor-hbar-face :underline `(:color ,highlight))

    ;; evil-snipe
    (evil-snipe-first-match-face :foreground highlight :background dark-blue :weight 'bold)
    (evil-snipe-matches-face     :foreground highlight :underline t :weight 'bold)

    ;; evil-googles
    (evil-goggles-default-face :inherit 'region)

    ;; flycheck
    (flycheck-error     :underline `(:style wave :color ,red))
    (flycheck-warning   :underline `(:style wave :color ,yellow))
    (flycheck-info      :underline `(:style wave :color ,green))

    ;; flycheck-posframe
    (flycheck-posframe-face :inherit 'default)
    (flycheck-posframe-background-face :background bg-alt)
    (flycheck-posframe-error-face   :inherit 'flycheck-posframe-face :foreground error)
    (flycheck-posframe-info-face    :inherit 'flycheck-posframe-face :foreground fg)
    (flycheck-posframe-warning-face :inherit 'flycheck-posframe-face :foreground warning)

    ;; flymake
    (flymake-error   :underline `(:style wave :color ,red))
    (flymake-note    :underline `(:style wave :color ,green))
    (flymake-warning :underline `(:style wave :color ,orange))

    ;; flyspell
    (flyspell-incorrect :underline `(:style wave :color ,error) :inherit 'unspecified)

    ;; git-commit
    (git-commit-summary :foreground strings)
    (git-commit-overlong-summary :inherit 'error :background base0 :slant 'italic :weight 'bold)
    (git-commit-nonempty-second-line :inherit 'git-commit-overlong-summary)
    (git-commit-note :foreground cyan :slant 'italic)
    (git-commit-pseudo-header :foreground doc-comments :slant 'italic)
    (git-commit-known-pseudo-header :foreground doc-comments :weight 'bold :slant 'italic)
    (git-commit-comment-branch-local :foreground magenta)
    (git-commit-comment-branch-remote :foreground green)
    (git-commit-comment-detached :foreground orange)
    (git-commit-comment-heading :foreground keywords)
    (git-commit-comment-file :foreground violet)
    (git-commit-comment-action)

    ;; git-gutter
    (git-gutter:modified :foreground cyan)
    (git-gutter:added    :foreground vc-added)
    (git-gutter:deleted  :foreground vc-deleted)

    ;; git-gutter+
    (git-gutter+-modified :foreground cyan :background nil)
    (git-gutter+-added    :foreground vc-added :background nil)
    (git-gutter+-deleted  :foreground vc-deleted :background nil)

    ;; git-gutter-fringe
    ((git-gutter-fr:modified &inherit git-gutter:modified))
    ((git-gutter-fr:added    &inherit git-gutter:added))
    ((git-gutter-fr:deleted  &inherit git-gutter:deleted))

    ;; gnus
    (gnus-group-mail-1           :weight 'bold :foreground fg)
    (gnus-group-mail-2           :inherit 'gnus-group-mail-1)
    (gnus-group-mail-3           :inherit 'gnus-group-mail-1)
    (gnus-group-mail-1-empty     :foreground base5)
    (gnus-group-mail-2-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-mail-3-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-1           :inherit 'gnus-group-mail-1)
    (gnus-group-news-2           :inherit 'gnus-group-news-1)
    (gnus-group-news-3           :inherit 'gnus-group-news-1)
    (gnus-group-news-4           :inherit 'gnus-group-news-1)
    (gnus-group-news-5           :inherit 'gnus-group-news-1)
    (gnus-group-news-6           :inherit 'gnus-group-news-1)
    (gnus-group-news-1-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-2-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-3-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-4-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-5-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-6-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-mail-low         :inherit 'gnus-group-mail-1 :weight 'normal)
    (gnus-group-mail-low-empty   :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-low         :inherit 'gnus-group-mail-1 :foreground base5)
    (gnus-group-news-low-empty   :inherit 'gnus-group-news-low :weight 'normal)
    (gnus-header-content         :inherit 'message-header-other)
    (gnus-header-from            :inherit 'message-header-other)
    (gnus-header-name            :inherit 'message-header-name)
    (gnus-header-newsgroups      :inherit 'message-header-other)
    (gnus-header-subject         :inherit 'message-header-subject)
    (gnus-summary-cancelled      :foreground red :strike-through t)
    (gnus-summary-high-ancient   :foreground (dotemacs-lighten base5 0.2) :inherit 'italic)
    (gnus-summary-high-read      :foreground (dotemacs-lighten fg 0.2))
    (gnus-summary-high-ticked    :foreground (dotemacs-lighten magenta 0.2))
    (gnus-summary-high-unread    :foreground (dotemacs-lighten green 0.2))
    (gnus-summary-low-ancient    :foreground (dotemacs-darken base5 0.2) :inherit 'italic)
    (gnus-summary-low-read       :foreground (dotemacs-darken fg 0.2))
    (gnus-summary-low-ticked     :foreground (dotemacs-darken magenta 0.2))
    (gnus-summary-low-unread     :foreground (dotemacs-darken green 0.2))
    (gnus-summary-normal-ancient :foreground base5 :inherit 'italic)
    (gnus-summary-normal-read    :foreground fg)
    (gnus-summary-normal-ticked  :foreground magenta)
    (gnus-summary-normal-unread  :foreground green :inherit 'bold)
    (gnus-summary-selected       :foreground blue :weight 'bold)
    (gnus-cite-1                 :foreground violet)
    (gnus-cite-2                 :foreground violet)
    (gnus-cite-3                 :foreground violet)
    (gnus-cite-4                 :foreground green)
    (gnus-cite-5                 :foreground green)
    (gnus-cite-6                 :foreground green)
    (gnus-cite-7                 :foreground magenta)
    (gnus-cite-8                 :foreground magenta)
    (gnus-cite-9                 :foreground magenta)
    (gnus-cite-10                :foreground yellow)
    (gnus-cite-11                :foreground yellow)
    (gnus-signature              :foreground yellow)
    (gnus-x-face                 :background base5 :foreground fg)

    ;; helm
    (helm-selection
     (&all :inherit 'bold :background selection)
     (&dark  :distant-foreground highlight)
     (&light :distant-foreground base0))
    (helm-match :inherit 'bold :foreground highlight :distant-foreground base8)
    (helm-source-header          :background base2 :foreground base5)
    (helm-swoop-target-line-face :foreground highlight :inverse-video t)
    (helm-visible-mark           :inherit '(bold highlight))
    (helm-ff-file                :foreground fg)
    (helm-ff-prefix              :foreground keywords)
    (helm-ff-dotted-directory    :foreground grey)
    (helm-ff-directory           :foreground variables)
    (helm-ff-executable          :foreground base8 :inherit 'italic)
    (helm-grep-match             :foreground highlight :distant-foreground red)
    (helm-grep-file              :foreground methods)
    (helm-grep-lineno            :foreground base5)
    (helm-grep-finish            :foreground green)
    (helm-swoop-target-line-face       :foreground highlight :inverse-video t)
    (helm-swoop-target-line-block-face :foreground yellow)
    (helm-swoop-target-word-face       :foreground green :inherit 'bold)
    (helm-swoop-target-number-face     :foreground base5)

    ;; helpful
    (helpful-heading :weight 'bold :height 1.2)

    ;; hideshow
    (+dotemacs-folded-face :inherit 'font-lock-comment-face
                       :weight 'light
                       :background (dotemacs-darken bg 0.125))

    ;; highlight-indentation-mode
    (highlight-indentation-face                :inherit 'hl-line)
    (highlight-indentation-current-column-face :background base1)
    (highlight-indentation-guides-odd-face     :inherit 'highlight-indentation-face)
    (highlight-indentation-guides-even-face    :inherit 'highlight-indentation-face)

    ;; highlight-quoted-mode
    (highlight-quoted-symbol :foreground type)
    (highlight-quoted-quote  :foreground operators)

    ;; highlight-numbers-mode
    (highlight-numbers-number :inherit 'bold :foreground numbers)

    ;; hlinum
    (linum-highlight-face :foreground fg :distant-foreground nil :weight 'normal)

    ;; hl-todo
    (hl-todo :foreground red :weight 'bold)

    ;; hydra
    (hydra-face-red      :foreground red     :weight 'bold)
    (hydra-face-blue     :foreground blue    :weight 'bold)
    (hydra-face-amaranth :foreground magenta :weight 'bold)
    (hydra-face-pink     :foreground violet  :weight 'bold)
    (hydra-face-teal     :foreground teal    :weight 'bold)

    ;; iedit
    (iedit-occurrence :foreground magenta :weight 'bold :inverse-video t)
    (iedit-read-only-occurrence :inherit 'region)

    ;; imenu-list
    ;; (imenu-list-entry-face)
    (imenu-list-entry-face-0 :foreground highlight)
    (imenu-list-entry-subalist-face-0 :inherit 'imenu-list-entry-face-0 :weight 'bold)
    (imenu-list-entry-face-1 :foreground green)
    (imenu-list-entry-subalist-face-1 :inherit 'imenu-list-entry-face-1 :weight 'bold)
    (imenu-list-entry-face-2 :foreground yellow)
    (imenu-list-entry-subalist-face-2 :inherit 'imenu-list-entry-face-2 :weight 'bold)

    ;; indent-guide
    ((indent-guide-face &inherit highlight-indentation-face))

    ;; ivy
    (ivy-current-match :background region :distant-foreground nil)
    (ivy-minibuffer-match-face-1
     :background nil
     :foreground (dotemacs-lighten grey 0.14)
     :weight 'light)
    (ivy-minibuffer-match-face-2
     :inherit 'ivy-minibuffer-match-face-1
     :foreground magenta :background base1 :weight 'semi-bold)
    (ivy-minibuffer-match-face-3
     :inherit 'ivy-minibuffer-match-face-2
     :foreground green :weight 'semi-bold)
    (ivy-minibuffer-match-face-4
     :inherit 'ivy-minibuffer-match-face-2
     :foreground yellow :weight 'semi-bold)
    (ivy-minibuffer-match-highlight :foreground violet)
    (ivy-highlight-face :foreground violet)
    (ivy-confirm-face :foreground success)
    (ivy-match-required-face :foreground error)
    (ivy-virtual :inherit 'italic :foreground doc-comments)
    (ivy-modified-buffer :inherit 'bold :foreground vc-modified)

    ;; ivy-posframe
    (ivy-posframe :background (dotemacs-darken bg-alt 0.2))

    ;; jabber
    (jabber-activity-face          :foreground red   :weight 'bold)
    (jabber-activity-personal-face :foreground blue  :weight 'bold)
    (jabber-chat-error             :foreground red   :weight 'bold)
    (jabber-chat-prompt-foreign    :foreground red   :weight 'bold)
    (jabber-chat-prompt-local      :foreground blue  :weight 'bold)
    (jabber-chat-prompt-system     :foreground green :weight 'bold)
    (jabber-chat-text-foreign      :foreground fg)
    (jabber-chat-text-local        :foreground fg)
    (jabber-rare-time-face         :foreground green)
    (jabber-roster-user-away       :foreground yellow)
    (jabber-roster-user-chatty     :foreground green :weight 'bold)
    (jabber-roster-user-dnd        :foreground red)
    (jabber-roster-user-error      :foreground red)
    (jabber-roster-user-offline    :foreground fg)
    (jabber-roster-user-online     :foreground green :weight 'bold)
    (jabber-roster-user-xa         :foreground cyan)

    ;; linum-relative
    ((linum-relative-current-face &inherit line-number-current-line))

    ;; lui
    (lui-time-stamp-face :foreground violet)
    (lui-highlight-face :foreground highlight)
    (lui-button-face :foreground builtin :underline t)

    ;; multiple cursors
    (mc/cursor-face :inherit 'cursor)

    ;; nav-flash
    (nav-flash-face :background selection :foreground base8 :weight 'bold)

    ;; neotree
    (neo-root-dir-face   :foreground strings :background bg :box `(:line-width 4 :color ,bg))
    (neo-file-link-face  :foreground fg)
    (neo-dir-link-face   :foreground highlight)
    (neo-expand-btn-face :foreground highlight)
    (neo-vc-edited-face  :foreground yellow)
    (neo-vc-added-face   :foreground green)
    (neo-vc-removed-face :foreground red :strike-through t)
    (neo-vc-conflict-face :foreground magenta :weight 'bold)
    (neo-vc-ignored-face  :foreground comments)
    (dotemacs-neotree-dir-face :foreground highlight)
    (dotemacs-neotree-file-face :foreground base8)
    (dotemacs-neotree-hidden-file-face :foreground comments)
    (dotemacs-neotree-text-file-face :foreground fg)
    (dotemacs-neotree-data-file-face :foreground violet)
    (dotemacs-neotree-media-file-face :inherit 'dotemacs-neotree-hidden-file-face)

    ;; nlinum
    ((nlinum-current-line &inherit line-number-current-line))

    ;; nlinum-hl
    ((nlinum-hl-face &inherit line-number-current-line))

    ;; nlinum-relative
    ((nlinum-relative-current-face &inherit line-number-current-line))

    ;; lsp
    ;; TODO Add light versions
    (lsp-face-highlight-textual :background dark-blue :foreground base8 :distant-foreground base0 :weight 'bold)
    (lsp-face-highlight-read    :background dark-blue :foreground base8 :distant-foreground base0 :weight 'bold)
    (lsp-face-highlight-write   :background dark-blue :foreground base8 :distant-foreground base0 :weight 'bold)
    (lsp-ui-peek-filename :inherit 'mode-line-buffer-id)
    (lsp-ui-peek-header :foreground fg :background (dotemacs-lighten bg 0.1) :bold bold)
    (lsp-ui-peek-selection :foreground bg :background blue :bold bold)
    (lsp-ui-peek-list :background (dotemacs-darken bg 0.1))
    (lsp-ui-peek-peek :background (dotemacs-darken bg 0.1))
    (lsp-ui-peek-highlight :inherit 'lsp-ui-peek-header :background region :foreground bg :box t)
    (lsp-ui-peek-line-number :foreground success)

    ;; magit
    (magit-bisect-bad        :foreground red)
    (magit-bisect-good       :foreground green)
    (magit-bisect-skip       :foreground orange)
    (magit-blame-date        :foreground red)
    (magit-blame-heading     :foreground orange :background base3)
    (magit-branch-current    :foreground blue)
    (magit-branch-local      :foreground cyan)
    (magit-branch-remote     :foreground green)
    (magit-cherry-equivalent :foreground violet)
    (magit-cherry-unmatched  :foreground cyan)
    (magit-diff-added             :foreground (dotemacs-darken green 0.2)  :background (dotemacs-blend green bg 0.1))
    (magit-diff-added-highlight   :foreground green                    :background (dotemacs-blend green bg 0.2) :weight 'bold)
    (magit-diff-base              :foreground (dotemacs-darken orange 0.2) :background (dotemacs-blend orange bg 0.1))
    (magit-diff-base-highlight    :foreground orange                   :background (dotemacs-blend orange bg 0.2) :weight 'bold)
    (magit-diff-context           :foreground (dotemacs-darken fg 0.4) :background bg)
    (magit-diff-context-highlight :foreground fg                   :background bg-alt)
    (magit-diff-file-heading           :foreground fg :weight 'bold)
    (magit-diff-file-heading-selection :foreground magenta               :background dark-blue :weight 'bold)
    (magit-diff-hunk-heading           :foreground bg                    :background (dotemacs-blend violet bg 0.3))
    (magit-diff-hunk-heading-highlight :foreground bg                    :background violet :weight 'bold)
    (magit-diff-removed                :foreground (dotemacs-darken red 0.2) :background (dotemacs-blend red base3 0.1))
    (magit-diff-removed-highlight      :foreground red                   :background (dotemacs-blend red base3 0.2) :weight 'bold)
    (magit-diff-lines-heading          :foreground yellow     :background red)
    (magit-diffstat-added              :foreground green)
    (magit-diffstat-removed            :foreground red)
    (magit-dimmed :foreground comments)
    (magit-hash :foreground comments)
    (magit-header-line :background dark-blue :foreground base8 :weight 'bold
                       :box `(:line-width 3 :color ,dark-blue))
    (magit-log-author :foreground orange)
    (magit-log-date :foreground blue)
    (magit-log-graph :foreground comments)
    (magit-process-ng :inherit 'error)
    (magit-process-ok :inherit 'success)
    (magit-reflog-amend :foreground magenta)
    (magit-reflog-checkout :foreground blue)
    (magit-reflog-cherry-pick :foreground green)
    (magit-reflog-commit :foreground green)
    (magit-reflog-merge :foreground green)
    (magit-reflog-other :foreground cyan)
    (magit-reflog-rebase :foreground magenta)
    (magit-reflog-remote :foreground cyan)
    (magit-reflog-reset :inherit 'error)
    (magit-refname :foreground comments)
    (magit-section-heading           :foreground blue :weight 'bold)
    (magit-section-heading-selection :foreground orange :weight 'bold)
    (magit-section-highlight :inherit 'hl-line)
    (magit-sequence-drop :foreground red)
    (magit-sequence-head :foreground blue)
    (magit-sequence-part :foreground orange)
    (magit-sequence-stop :foreground green)
    (magit-signature-bad :inherit 'error)
    (magit-signature-error :inherit 'error)
    (magit-signature-expired :foreground orange)
    (magit-signature-good :inherit 'success)
    (magit-signature-revoked :foreground magenta)
    (magit-signature-untrusted :foreground yellow)
    (magit-tag :foreground yellow)
    (magit-filename :foreground violet)
    (magit-section-secondary-heading :foreground violet :weight 'bold)

    ;; mic-paren
    (paren-face-match    :foreground red   :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)

    ;; parenface
    (paren-face :foreground comments)

    ;; perspective
    (persp-selected-face :foreground blue :weight 'bold)

    ;; persp-mode
    (persp-face-lighter-buffer-not-in-persp :foreground warning :slant 'italic)
    (persp-face-lighter-nil-persp :foreground comments)

    ;; popup
    (popup-face :inherit 'tooltip)
    (popup-tip-face :inherit 'popup-face :foreground violet :background base0)
    (popup-selection-face :background selection)
    
    ;; power
    (powerline-active0   :inherit 'mode-line :background bg)
    (powerline-active1   :inherit 'mode-line :background (dotemacs-lighten 'bg 0.025))
    (powerline-active2   :inherit 'mode-line :foreground base8 :background (dotemacs-lighten 'bg 0.08))
    (powerline-inactive0 :inherit 'mode-line-inactive :background base2)
    (powerline-inactive1 :inherit 'mode-line-inactive :background (dotemacs-lighten 'base2 0.02))
    (powerline-inactive2 :inherit 'mode-line-inactive :background (dotemacs-lighten 'base2 0.04))

    ;; rainbow-delimiters
    (rainbow-delimiters-depth-1-face :foreground blue)
    (rainbow-delimiters-depth-2-face :foreground magenta)
    (rainbow-delimiters-depth-3-face :foreground green)
    (rainbow-delimiters-depth-4-face :foreground orange)
    (rainbow-delimiters-depth-5-face :foreground violet)
    (rainbow-delimiters-depth-6-face :foreground yellow)
    (rainbow-delimiters-depth-7-face :foreground teal)
    (rainbow-delimiters-unmatched-face  :foreground red :weight 'bold :inverse-video t)
    (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)

    ;; re-builder
    (reb-match-0 :foreground orange  :inverse-video t)
    (reb-match-1 :foreground magenta :inverse-video t)
    (reb-match-2 :foreground green   :inverse-video t)
    (reb-match-3 :foreground yellow  :inverse-video t)

    ;; show-paren
    ((show-paren-match &inherit paren-face-match))
    ((show-paren-mismatch &inherit paren-face-mismatch))

    ;; smartparens
    (sp-pair-overlay-face :background region)

    ;; smartparens
    ((sp-show-pair-match-face    &inherit show-paren-match))
    ((sp-show-pair-mismatch-face &inherit show-paren-mismatch))

    ;; smerge-tool
    (smerge-lower :background (dotemacs-blend green bg 0.2))
    (smerge-upper :background (dotemacs-blend red base3 0.2))
    (smerge-base  :background (dotemacs-blend blue bg 0.2))
    (smerge-markers :background comments :foreground bg :distant-foreground fg :weight 'bold)
    ;; Emacs <25 compatibility
    ((smerge-mine  &inherit smerge-upper))
    ((smerge-other &inherit smerge-lower))

    ;; solaire-mode
    (solaire-default-face  :inherit 'default :background bg-alt)
    (solaire-hl-line-face  :inherit 'hl-line :background bg)
    (solaire-org-hide-face :foreground bg-alt)

    ;; stripe-buffer
    (stripe-highlight
     (&light :background base5)
     (&dark  :background base3))

    ;; swiper
    (swiper-line-face    :background blue    :foreground base0)
    (swiper-match-face-1 :inherit 'unspecified :background base0   :foreground base5)
    (swiper-match-face-2 :inherit 'unspecified :background orange  :foreground base0 :weight 'bold)
    (swiper-match-face-3 :inherit 'unspecified :background magenta :foreground base0 :weight 'bold)
    (swiper-match-face-4 :inherit 'unspecified :background green   :foreground base0 :weight 'bold)

    ;; tabbar
    (tabbar-default             :foreground bg :background bg :height 1.0)
    (tabbar-highlight           :foreground fg :background selection :distant-foreground bg)
    (tabbar-button              :foreground fg :background bg)
    (tabbar-button-highlight    :inherit 'tabbar-button :inverse-video t)
    (tabbar-modified            :inherit 'tabbar-default :foreground red :weight 'bold)
    (tabbar-unselected          :inherit 'tabbar-default :foreground base5)
    (tabbar-unselected-modified :inherit 'tabbar-modified)
    (tabbar-selected
     :inherit 'tabbar-default :weight 'bold
     :foreground fg :background bg-alt)
    (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)

    ;; tldr
    (tldr-command-itself   :foreground bg :background green :weight 'semi-bold)
    (tldr-title            :foreground yellow :bold t :height 1.4)
    (tldr-description      :foreground fg :weight 'semi-bold)
    (tldr-introduction     :foreground (dotemacs-blend blue bg 0.8) :weight 'semi-bold)
    (tldr-code-block       :foreground green :background region :weight 'semi-bold)
    (tldr-command-argument :foreground fg :background region )

    ;; treemacs
    (treemacs-root-face :inherit 'font-lock-string-face :weight 'bold :height 1.2)
    (treemacs-file-face :foreground fg)
    (treemacs-directory-face :foreground fg)
    (treemacs-tags-face :foreground highlight)
    (treemacs-git-modified-face :foreground violet)
    (treemacs-git-added-face :foreground green)
    (treemacs-git-conflict-face :foreground red)
    (treemacs-git-untracked-face :inherit 'font-lock-doc-face)

    ;; twittering-mode
    (twitter-divider  ; custom face in dotemacs Emacs
     (&light :underline '(:color (dotemacs-lighten vertical-bar 0.2)))
     (&dark  :underline '(:color (dotemacs-darken vertical-bar 0.2))))

    ;; undo-tree
    (undo-tree-visualizer-default-face :foreground base5)
    (undo-tree-visualizer-current-face :foreground green :weight 'bold)
    (undo-tree-visualizer-unmodified-face :foreground base5)
    (undo-tree-visualizer-active-branch-face :foreground blue)
    (undo-tree-visualizer-register-face :foreground yellow)

    ;; vimish-fold
    (vimish-fold-overlay :inherit 'font-lock-comment-face :background base0 :weight 'light)
    (vimish-fold-fringe  :foreground magenta)

    ;; volatile-highlights
    (vhl/default-face :background grey)

    ;; wgrep
    (wgrep-face :weight 'bold :foreground green :background base5)
    (wgrep-delete-face :foreground base3 :background red)
    (wgrep-done-face   :foreground blue)
    (wgrep-file-face   :foreground comments)
    (wgrep-reject-face :foreground red :weight 'bold)

    ;; which-func
    (which-func :foreground blue)

    ;; which-key
    (which-key-key-face                   :foreground green)
    (which-key-group-description-face     :foreground violet)
    (which-key-command-description-face   :foreground blue)
    (which-key-local-map-description-face :foreground magenta)

    ;; whitespace
    (whitespace-empty    :background base3)
    (whitespace-space    :foreground base4)
    (whitespace-tab      :foreground base4 :background (unless (default-value 'indent-tabs-mode) base3))
    (whitespace-newline  :foreground base4)
    (whitespace-indentation :foreground red :background yellow)
    (whitespace-trailing :inherit 'trailing-whitespace)
    (whitespace-line     :background base0 :foreground red :weight 'bold)

    ;; workgroups2
    (wg-current-workgroup-face :foreground base0 :background highlight)
    (wg-other-workgroup-face   :foreground base5)
    (wg-divider-face           :foreground grey)
    (wg-brace-face             :foreground highlight)

    ;; yasnippet
    (yas-field-highlight-face :inherit 'match)


    ;; --- major-mode faces -------------------
    ;; agda2-mode
    (agda2-highlight-keyword-face                 :inherit 'font-lock-keyword-face)
    (agda2-highlight-string-face                  :inherit 'font-lock-string-face)
    (agda2-highlight-number-face                  :inherit 'font-lock-string-face)
    (agda2-highlight-symbol-face                  :inherit 'font-lock-variable-name-face)
    (agda2-highlight-primitive-type-face          :inherit 'font-lock-type-face)
    (agda2-highlight-bound-variable-face          :inherit 'font-lock-variable-name-face)
    (agda2-highlight-inductive-constructor-face   :inherit 'font-lock-type-face)
    (agda2-highlight-coinductive-constructor-face :inherit 'font-lock-type-face)
    (agda2-highlight-datatype-face                :inherit 'font-lock-type-face)
    (agda2-highlight-field-face                   :inherit 'font-lock-type-face)
    (agda2-highlight-function-face                :inherit 'font-lock-function-name-face)
    (agda2-highlight-module-face                  :inherit 'font-lock-variable-name-face)
    (agda2-highlight-postulate-face               :inherit 'font-lock-type-face)
    (agda2-highlight-primitive-face               :inherit 'font-lock-type-face)
    (agda2-highlight-macro-face                   :inherit 'font-lock-function-name-face)
    (agda2-highlight-record-face                  :inherit 'font-lock-type-face)
    (agda2-highlight-error-face                   :inherit 'font-lock-warning-face)
    (agda2-highlight-dotted-face                  :inherit 'font-lock-variable-name-face)
    (agda2-highlight-unsolved-meta-face           :inherit 'font-lock-warning-face)
    (agda2-highlight-unsolved-constraint-face     :inherit 'font-lock-warning-face)
    (agda2-highlight-termination-problem-face     :inherit 'font-lock-warning-face)
    (agda2-highlight-positivity-problem-face      :inherit 'font-lock-warning-face)
    (agda2-highlight-incomplete-pattern-face      :inherit 'font-lock-warning-face)
    (agda2-highlight-typechecks-face              :inherit 'font-lock-warning-face)

    ;; auctex (latex-mode)
    (font-latex-bold-face         :inherit 'bold)
    (font-latex-italic-face       :inherit 'italic)
    (font-latex-math-face         :foreground blue)
    (font-latex-sectioning-0-face :foreground blue    :weight 'ultra-bold)
    (font-latex-sectioning-1-face :foreground magenta :weight 'semi-bold)
    (font-latex-sectioning-2-face :foreground violet  :weight 'semi-bold)
    (font-latex-sectioning-3-face :foreground (dotemacs-lighten blue 0.3)    :weight 'semi-bold)
    (font-latex-sectioning-4-face :foreground (dotemacs-lighten magenta 0.3) :weight 'semi-bold)
    (font-latex-sectioning-5-face :foreground (dotemacs-lighten violet 0.3)  :weight 'semi-bold)
    (font-latex-script-char-face  :foreground dark-blue)
    (font-latex-string-face       :inherit 'font-lock-string-face)
    (font-latex-warning-face      :inherit 'font-lock-warning-face)
    (font-latex-verbatim-face     :inherit 'fixed-pitch :foreground violet :slant 'italic)

    ;; elixir-mode
    (elixir-atom-face (&light :foreground dark-blue)
                      (&dark  :foreground cyan))
    (elixir-attribute-face :foreground violet)

    ;; enh-ruby-mode
    (enh-ruby-op-face :foreground operators)
    (enh-ruby-string-delimiter-face  :inherit 'font-lock-string-face)
    (enh-ruby-heredoc-delimiter-face :inherit 'font-lock-string-face)
    (enh-ruby-regexp-face :foreground constants)
    (enh-ruby-regexp-delimiter-face  :inherit 'enh-ruby-regexp-face)
    (erm-syn-errline  :underline `(:style wave :color ,error))
    (erm-syn-warnline :underline `(:style wave :color ,warning))

    ;; jdee-mode
    (jdee-font-lock-number-face :foreground numbers)
    (jdee-font-lock-operator-face :foreground operators)
    (jdee-font-lock-constant-face :inherit 'font-lock-constant-face)
    (jdee-font-lock-constructor-face :foreground methods)
    (jdee-font-lock-public-face :inherit 'font-lock-keyword-face)
    (jdee-font-lock-protected-face :inherit 'font-lock-keyword-face)
    (jdee-font-lock-private-face :inherit 'font-lock-keyword-face)
    (jdee-font-lock-modifier-face :inherit 'font-lock-type-face)
    (jdee-font-lock-doc-tag-face :foreground violet)
    (jdee-font-lock-italic-face :inherit 'italic)
    (jdee-font-lock-bold-face :inherit 'bold)
    (jdee-font-lock-link-face :foreground blue :italic nil :underline t)

    ;; js2-mode
    (js2-function-param  :foreground variables)
    (js2-function-call   :foreground functions)
    (js2-object-property :foreground violet)
    (js2-jsdoc-tag       :foreground doc-comments)

    ;; ledger-mode
    (ledger-font-posting-date-face :foreground blue)
    (ledger-font-posting-amount-face :foreground yellow)
    (ledger-font-posting-account-face :foreground base8)
    (ledger-font-payee-cleared-face :foreground violet :weight 'bold :height 1.2)
    (ledger-font-payee-uncleared-face :foreground base5 :weight 'bold :height 1.2)
    (ledger-font-xact-highlight-face :background base0)

    ;; makefile-*-mode
    (makefile-targets :foreground blue)

    ;; markdown-mode
    (markdown-header-face           :inherit 'bold :foreground highlight)
    (markdown-header-delimiter-face :inherit 'markdown-header-face)
    (markdown-metadata-key-face     :foreground red)
    (markdown-list-face             :foreground red)
    (markdown-link-face             :foreground highlight)
    (markdown-url-face              :foreground magenta :weight 'normal)
    (markdown-italic-face           :inherit 'italic :foreground violet)
    (markdown-bold-face             :inherit 'bold   :foreground orange)
    (markdown-markup-face           :foreground operators)
    (markdown-blockquote-face       :inherit 'italic :foreground doc-comments)
    (markdown-pre-face              :foreground strings)
    (markdown-code-face :background base3)
    (markdown-reference-face        :foreground doc-comments)
    (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face))
    (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
    (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
    (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
    (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
    (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)

    ;; notmuch
    ;; (notmuch-crypto-decryption               :foreground blue-l)
    ;; (notmuch-crypto-part-header              :foreground yellow-l)
    ;; (notmuch-crypto-signature-bad            :foreground red-l)
    ;; (notmuch-crypto-signature-good           :foreground base1)
    ;; (notmuch-crypto-signature-good-key       :foreground aqua-l)
    ;; (notmuch-crypto-signature-unknown        :foreground yellow)
    ;; (notmuch-hello-logo-background           :foreground fg)
    (notmuch-message-summary-face            :foreground grey :background nil)
    (notmuch-search-count                    :foreground comments)
    (notmuch-search-date                     :foreground numbers :weight 'bold)
    (notmuch-search-flagged-face             :foreground (dotemacs-blend red base4 0.5))
    (notmuch-search-matching-authors         :foreground blue :weight 'bold)
    (notmuch-search-non-matching-authors     :foreground blue)
    (notmuch-search-subject                  :foreground fg)
    (notmuch-search-unread-face              :foreground base8)
    (notmuch-tag-added                       :foreground green :weight 'normal)
    (notmuch-tag-deleted                     :foreground red :weight 'normal)
    (notmuch-tag-face                        :foreground yellow :weight 'normal)
    (notmuch-tag-flagged                     :foreground yellow :weight 'normal)
    (notmuch-tag-unread                      :foreground yellow :weight 'normal)
    (notmuch-tree-match-author-face          :foreground blue :weight 'bold)
    (notmuch-tree-match-date-face            :foreground numbers :weight 'bold)
    (notmuch-tree-match-face                 :foreground fg)
    (notmuch-tree-match-subject-face         :foreground fg)
    (notmuch-tree-match-tag-face             :foreground yellow)
    (notmuch-tree-match-tree-face            :foreground comments)
    (notmuch-tree-no-match-author-face       :foreground blue)
    (notmuch-tree-no-match-date-face         :foreground numbers)
    (notmuch-tree-no-match-face              :foreground base5)
    (notmuch-tree-no-match-subject-face      :foreground base5)
    (notmuch-tree-no-match-tag-face          :foreground yellow)
    (notmuch-tree-no-match-tree-face         :foreground yellow)
    (notmuch-wash-cited-text                 :foreground base4)
    (notmuch-wash-toggle-button :foreground fg)

    ;; outline
    (outline-1 :foreground blue     :background base3  :weight 'bold :height 1.25)
    (outline-2 :foreground magenta                     :weight 'bold)
    (outline-3 :foreground violet                      :weight 'bold)
    (outline-4 :foreground (dotemacs-lighten blue 0.25)    :weight 'bold)
    (outline-5 :foreground (dotemacs-lighten magenta 0.25) :weight 'bold)
    (outline-6 :foreground (dotemacs-lighten blue 0.5)     :weight 'bold)
    (outline-7 :foreground (dotemacs-lighten magenta 0.5)  :weight 'bold)
    (outline-8 :foreground (dotemacs-lighten blue 0.8)     :weight 'bold)

    ;; org-mode
    (org-archived                 :foreground doc-comments)
    (org-block                    :background base3)
    (org-block-background         :background base3)
    (org-block-begin-line         :foreground comments :background base3)
    (org-block-end-line           :inherit 'org-block-begin-line)
    (org-checkbox                 :inherit 'org-todo)
    (org-checkbox-statistics-done :inherit 'org-done)
    (org-checkbox-statistics-todo :inherit 'org-todo)
    (org-code                     :foreground orange)
    (org-date                     :foreground yellow)
    (org-default                  :inherit 'variable-pitch)
    (org-document-info            :foreground builtin)
    (org-document-title           :foreground builtin :weight 'bold)
    (org-done                     :inherit 'org-headline-done :bold 'inherit)
    (org-ellipsis                 :underline nil :background nil :foreground grey)
    (org-footnote                 :foreground orange)
    (org-formula                  :foreground cyan)
    (org-headline-done            :foreground base5)
    (org-hide                     :foreground bg)

    ;; extends from outline-N
    ;; (org-level-1)
    ;; (org-level-2)
    ;; (org-level-3)
    ;; (org-level-4)
    ;; (org-level-5)
    ;; (org-level-6)
    ;; (org-level-7)
    ;; (org-level-8)

    (org-list-dt         :foreground highlight)
    (org-meta-line       :foreground doc-comments)
    (org-priority        :foreground red)
    (org-property-value  :foreground doc-comments)
    (org-quote           :background base3 :slant 'italic)
    (org-special-keyword :foreground doc-comments)
    (org-table           :foreground violet)
    (org-tag             :foreground doc-comments :weight 'normal)
    (org-ref-cite-face   :foreground yellow :weight 'light :underline t)
    (org-todo            :foreground green :bold 'inherit)
    (org-verbatim        :foreground green)
    (org-warning         :foreground warning)

    ;; org-agenda
    (org-agenda-done :inherit 'org-done)
    (org-agenda-dimmed-todo-face :foreground comments)
    (org-agenda-date          :foreground violet :weight 'ultra-bold)
    (org-agenda-date-today    :foreground (dotemacs-lighten violet 0.4)   :weight 'ultra-bold)
    (org-agenda-date-weekend  :foreground (dotemacs-darken violet 0.4)  :weight 'ultra-bold)
    (org-agenda-structure     :foreground fg :weight 'ultra-bold)
    (org-agenda-clocking      :background (dotemacs-blend blue bg 0.2))
    (org-upcoming-deadline         :foreground (dotemacs-blend fg bg 0.8))
    (org-upcoming-distant-deadline :foreground (dotemacs-blend fg bg 0.5))
    (org-scheduled            :foreground fg)
    (org-scheduled-today      :foreground base7)
    (org-scheduled-previously :foreground base8)
    (org-time-grid            :foreground comments)
    (org-sexp-date            :foreground fg)

    ;; org-habit
    (org-habit-clear-face          :weight 'bold :background base4)
    (org-habit-clear-future-face   :weight 'bold :background base3)
    (org-habit-ready-face          :weight 'bold :background (dotemacs-blend blue bg-alt 0.5))
    (org-habit-ready-future-face   :weight 'bold :background (dotemacs-blend blue bg-alt 0.3))
    (org-habit-alert-face          :weight 'bold :background (dotemacs-blend yellow bg-alt 0.5))
    (org-habit-alert-future-face   :weight 'bold :background (dotemacs-blend yellow bg-alt 0.3))
    (org-habit-overdue-face        :weight 'bold :background (dotemacs-blend red bg-alt 0.5))
    (org-habit-overdue-future-face :weight 'bold :background (dotemacs-blend red bg-alt 0.3))

    ;; rpm-spec-mode
    (rpm-spec-macro-face        :foreground yellow)
    (rpm-spec-var-face          :foreground violet)
    (rpm-spec-tag-face          :foreground blue)
    (rpm-spec-obsolete-tag-face :foreground red)
    (rpm-spec-package-face      :foreground orange)
    (rpm-spec-dir-face          :foreground green)
    (rpm-spec-doc-face          :foreground orange)
    (rpm-spec-ghost-face        :foreground comments)
    (rpm-spec-section-face      :foreground magenta)
    
    ;; rst-mode
    (rst-block :inherit 'font-lock-constant-face)
    (rst-level-1 :inherit 'rst-adornment :weight 'bold)
    (rst-level-2 :inherit 'rst-adornment :weight 'bold)
    (rst-level-3 :inherit 'rst-adornment :weight 'bold)
    (rst-level-4 :inherit 'rst-adornment :weight 'bold)
    (rst-level-5 :inherit 'rst-adornment :weight 'bold)
    (rst-level-6 :inherit 'rst-adornment :weight 'bold)

    ;; typescript-mode
    (typescript-jsdoc-tag :foreground doc-comments)
    (typescript-jsdoc-type :foreground (dotemacs-darken doc-comments 0.15))
    (typescript-jsdoc-value :foreground (dotemacs-lighten doc-comments 0.15))

    ;; sh-mode
    (sh-heredoc :inherit 'font-lock-string-face :weight 'normal)
    (sh-quoted-exec :inherit 'font-lock-preprocessor-face)

    ;; web-mode
    (web-mode-doctype-face           :foreground comments)
    (web-mode-html-tag-face          :foreground methods)
    (web-mode-html-tag-bracket-face  :foreground methods)
    (web-mode-html-attr-name-face    :foreground type)
    (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
    (web-mode-block-control-face     :foreground orange)
    (web-mode-html-tag-bracket-face  :foreground operators))
  "TODO")

(defvar dotemacs-themes-common-vars
  '((ansi-color-names-vector
     (vconcat (mapcar #'dotemacs-color '(base0 red green yellow blue magenta cyan base8))))

    (fci-rule-color (dotemacs-color 'base5))

    (jdee-db-spec-breakpoint-face-colors `(cons ,(dotemacs-color 'base0) ,(dotemacs-color 'grey)))
    (jdee-db-requested-breakpoint-face-colors `(cons ,(dotemacs-color 'base0) ,(dotemacs-color 'green)))
    (jdee-db-active-breakpoint-face-colors `(cons ,(dotemacs-color 'base0) ,(dotemacs-color 'highlight)))

    (vc-annotate-color-map
     `(list (cons 20  ,(dotemacs-color 'green))
            (cons 40  ,(dotemacs-blend 'yellow 'green (/ 1.0 3)))
            (cons 60  ,(dotemacs-blend 'yellow 'green (/ 2.0 3)))
            (cons 80  ,(dotemacs-color 'yellow))
            (cons 100 ,(dotemacs-blend 'orange 'yellow (/ 1.0 3)))
            (cons 120 ,(dotemacs-blend 'orange 'yellow (/ 2.0 3)))
            (cons 140 ,(dotemacs-color 'orange))
            (cons 160 ,(dotemacs-blend 'magenta 'orange (/ 1.0 3)))
            (cons 180 ,(dotemacs-blend 'magenta 'orange (/ 2.0 3)))
            (cons 200 ,(dotemacs-color 'magenta))
            (cons 220 ,(dotemacs-blend 'red 'magenta (/ 1.0 3)))
            (cons 240 ,(dotemacs-blend 'red 'magenta (/ 2.0 3)))
            (cons 260 ,(dotemacs-color 'red))
            (cons 280 ,(dotemacs-blend 'grey 'red (/ 1.0 4)))
            (cons 300 ,(dotemacs-blend 'grey 'red (/ 2.0 4)))
            (cons 320 ,(dotemacs-blend 'grey 'red (/ 3.0 4)))
            (cons 340 ,(dotemacs-color 'base5))
            (cons 360 ,(dotemacs-color 'base5))))
    (vc-annotate-very-old-color nil)
    (vc-annotate-background (dotemacs-color 'bg)))
  "TODO")


;;
;; Library
;;

(defvar dotemacs-themes--colors)
(defvar dotemacs--min-colors '(257 256 16))
(defvar dotemacs--quoted-p nil)
(defvar dotemacs-themes--faces nil)

(defun dotemacs-themes--colors-p (item)
  (declare (pure t) (side-effect-free t))
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote dotemacs-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((dotemacs--quoted-p t))
                      (dotemacs-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (dotemacs--quoted-p)
                      (dotemacs-themes--colors-p (cdr item))))

                   ((or (dotemacs-themes--colors-p car)
                        (dotemacs-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not dotemacs--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item dotemacs-themes--colors))))))

(defun dotemacs-themes--apply-faces (new-faces &optional default-faces)
  (declare (pure t) (side-effect-free t))
  (let ((default-faces (or default-faces dotemacs-themes-common-faces))
        (faces (make-hash-table :test #'eq :size (+ (length default-faces) (length new-faces))))
        (directives (make-hash-table :test #'eq)))
    (dolist (spec (append (mapcar #'copy-sequence default-faces) new-faces))
      (if (listp (car spec))
          (cl-destructuring-bind (face action &optional arg) (car spec)
            (unless (assq face new-faces)
              (puthash face (list action arg (cdr spec))
                       directives)))
        (puthash (car spec) (cdr spec) faces)))
    (cl-loop for face being the hash-keys of directives
             for (action target spec) = (gethash face directives)
             unless (memq action '(&inherit &extend &override))
             do (error "Invalid operation (%s) for '%s' face" action face)
             if (eq (car spec) 'quote)
             do (error "Can't extend literal face spec (for '%s')" face)
             ;; TODO Add &all/&light/&dark extension support
             else if (memq (car spec) '(&all &light &dark))
             do (error "Can't extend face with &all, &light or &dark specs (for '%s')" face)
             else do
             (puthash face
                      (let ((old-spec (gethash (or target face) faces))
                            (plist spec))
                        ;; remove duplicates
                        (while (keywordp (car plist))
                          (setq old-spec (plist-put old-spec (car plist) (cadr plist))
                                plist (cddr plist)))
                        old-spec)
                      faces))
    (let (results)
      (maphash (lambda (face plist)
                 (when (keywordp (car plist))
                   ;; TODO Clean up duplicates in &all/&light/&dark blocks
                   (dolist (prop (append (unless dotemacs-themes-enable-bold   '(:weight normal :bold nil))
                                         (unless dotemacs-themes-enable-italic '(:slant normal :italic nil))))
                     (when (and (plist-member plist prop)
                                (not (eq (plist-get plist prop) 'inherit)))
                       (plist-put plist prop
                                  (if (memq prop '(:weight :slant))
                                      (quote 'normal))))))
                 (push (cons face plist) results))
               faces)
      (nreverse results))))

(defun dotemacs-themes--colorize (item type)
  (declare (pure t) (side-effect-free t))
  (when item
    (let ((dotemacs--quoted-p dotemacs--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote dotemacs-color))
                    item)
                   ((eq (car item) 'dotemacs-ref)
                    (dotemacs-themes--colorize
                     (apply #'dotemacs-ref (cdr item)) type))
                   ((let* ((item (append item nil))
                           (car (car item))
                           (dotemacs--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t dotemacs--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (dotemacs-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not dotemacs--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item dotemacs-themes--colors))
             `(dotemacs-color ',item ',type))

            (item)))))

(defun dotemacs-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                (if (dotemacs-themes--colors-p real-attrs)
                    (dolist (cl dotemacs--min-colors `(list ,@(nreverse defs)))
                      (push `(list '((class color) (min-colors ,cl))
                                   (list ,@(dotemacs-themes--colorize real-attrs cl)))
                            defs))
                  `(list (list 't (list ,@real-attrs))))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))

             ((let (all-attrs defs)
                (dolist (attrs face-body `(list ,@(nreverse defs)))
                  (cond ((eq (car attrs) '&all)
                         (setq all-attrs (append all-attrs (cdr attrs))))

                        ((memq (car attrs) '(&dark &light))
                         (let ((bg (if (eq (car attrs) '&dark) 'dark 'light))
                               (real-attrs (append all-attrs (cdr attrs) '())))
                           (cond ((dotemacs-themes--colors-p real-attrs)
                                  (dolist (cl dotemacs--min-colors)
                                    (push `(list '((class color) (min-colors ,cl) (background ,bg))
                                                 (list ,@(dotemacs-themes--colorize real-attrs cl)))
                                          defs)))

                                 ((push `(list '((background ,bg)) (list ,@real-attrs))
                                        defs)))))))))))))


;;
;; Public functions
;;

(defun dotemacs-themes-prepare-facelist (custom-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (setq dotemacs-themes--faces (dotemacs-themes--apply-faces custom-faces))
  (mapcar #'dotemacs-themes--build-face dotemacs-themes--faces))

(defun dotemacs-themes-prepare-varlist (vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (declare (pure t) (side-effect-free t))
  (cl-loop for (var val) in (append dotemacs-themes-common-vars vars)
           collect `(list ',var ,val)))

(provide 'dotemacs-themes-common)
;;; dotemacs-themes-common.el ends here
