;; dotemacs-gruvbox-theme.el --- inspired by morhetz Gruvbox
(require 'dotemacs-themes)

;;
(defgroup dotemacs-gruvbox-theme nil
  "Options for dotemacs-gruvbox."
  :group 'dotemacs-themes)

(defcustom dotemacs-gruvbox-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'dotemacs-gruvbox-theme
  :type 'boolean)

(defcustom dotemacs-gruvbox-padded-modeline dotemacs-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'dotemacs-gruvbox-theme
  :type '(choice integer boolean))

;;
(def-dotemacs-theme dotemacs-gruvbox
  "Dark theme with pastel 'retro groove' colors."

  ;; name        gui       256       16
  ((bg         '("#282828" "#282828"  nil         ))
   (bg-alt     '("#323232" "#323232"  nil         ))
   (accent     '("#504945" "#504945" "brown"      ))

   (base0      '("#1B2229" "black"   "black"      ))
   (base1      '("#151617" "#101010" "brightblack"))
   (base2      '("#1d1f20" "#191919" "brightblack"))
   (base3      '("#2d2e2e" "#252525" "brightblack"))
   (base4      '("#4e4e4e" "#454545" "brightblack"))
   (base5      '("#555556" "#6b6b6b" "brightblack"))
   (base6      '("#7c6f64" "#7b7b7b" "brightblack"))
   (base7      '("#cfc0c5" "#c1c1c1" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "brightwhite"))
   (fg         '("#ebdbb2" "#dfdfdf" "brightwhite"))
   (fg-alt     '("#928374" "#dfdfdf" "brightwhite"))

   (grey       '("#555556" "#515154" "brightblack"))
   (red        '("#fb4934" "#e74c3c" "red"))
   (magenta    '("#fb2874" "#fb2874" "magenta"))
   (violet     '("#d3869b" "#d3869b" "brightmagenta"))
   (orange     '("#fe8019" "#fd971f" "brightred"))
   (yellow     '("#fabd2f" "#fabd2f" "yellow"))
   (dark-green '("#689d6a" "#689d6a" "green"))
   (green      '("#8ec07c" "#8ec07c" "green"))
   (teal       green)
   (olive      '("#b8bb26" "#b8bb26" "green"))
   (blue       '("#268bd2" "#2686D6" "brightblue"))
   (dark-blue  '("#727280" "#727280" "blue"))
   (cyan       '("#83a598" "#83a598" "brightcyan"))
   (dark-cyan  '("#458588" "#458588" "cyan"))

   ;; face categories
   (highlight      yellow)
   (vertical-bar   grey)
   (selection      accent)
   (builtin        orange)
   (comments       (if dotemacs-gruvbox-brighter-comments magenta base6))
   (doc-comments   (if dotemacs-gruvbox-brighter-comments (dotemacs-lighten magenta 0.2) (dotemacs-lighten fg-alt 0.25)))
   (constants      yellow)
   (functions      green)
   (keywords       red)
   (methods        green)
   (operators      cyan)
   (type           green)
   (strings        olive)
   (variables      cyan)
   (numbers        violet)
   (region         accent)
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    accent)
   (vc-added       (dotemacs-darken green 0.15))
   (vc-deleted     red)

   ;; custom categories
   (-modeline-pad
    (when dotemacs-gruvbox-padded-modeline
      (if (integerp dotemacs-gruvbox-padded-modeline)
          dotemacs-gruvbox-padded-modeline
        4)))

   (org-quote `(,(dotemacs-lighten (car bg) 0.05) "#1f1f1f")))

  ;; --- extra faces ------------------------
(
   ;;;;;;;; Editor ;;;;;;;;
   (cursor :background "white")
   (hl-line :background bg-alt)
   ((line-number-current-line &override) :background grey :foreground "white" :bold t)
   ((line-number &override) :foreground grey)

   ;; Vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background accent :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground "white" :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)

   ;;;;;;;; Doom-modeline ;;;;;;;;
   (mode-line
    :background accent :foreground (dotemacs-lighten fg-alt 0.25)
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base3)))

   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base2)))

   ;; File-name
   (dotemacs-modeline-project-dir :bold t :foreground cyan)
   (dotemacs-modeline-buffer-path :inherit 'bold :foreground green)
   (dotemacs-modeline-buffer-file :inherit 'bold :foreground fg)
   (dotemacs-modeline-buffer-modified :inherit 'bold :foreground yellow)
   ;; Misc
   (dotemacs-modeline-error :background bg)
   (dotemacs-modeline-buffer-major-mode :foreground green :bold t)
   (dotemacs-modeline-warning :foreground red :bold t)
   (dotemacs-modeline-info :bold t :foreground cyan)
   (dotemacs-modeline-bar :background dark-green)
   (dotemacs-modeline-panel :background dark-green :foreground fg)

   ;;;;;;;; Search ;;;;;;;;
   ;; /find
   (isearch :foreground base0 :background yellow)
   (evil-search-highlight-persist-highlight-face :background orange)
   (lazy-highlight :background yellow :foreground base0 :distant-foreground base0 :bold bold)
   (evil-ex-substitute-replacement :foreground yellow :inherit 'evil-ex-substitute-matches)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground "white" :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)

   ;;;;;;;; Mini-buffers ;;;;;;;;
   (minibuffer-prompt :foreground green)
   (solaire-hl-line-face :background accent)

   ;; ivy
   (ivy-current-match :background accent)
   (ivy-subdir :background nil :foreground cyan)
   (ivy-action :background nil :foreground cyan)
   (ivy-grep-line-number :background nil :foreground cyan)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow)
   (ivy-minibuffer-match-face-2 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground olive)
   (counsel-key-binding :foreground green)

   ;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)

   ;; neotree
   (neo-root-dir-face   :foreground green )
   (dotemacs-neotree-dir-face :foreground cyan)
   (neo-dir-link-face   :foreground cyan)
   (dotemacs-neotree-file-face :foreground fg)
   (dotemacs-neotree-hidden-file-face :foreground (dotemacs-lighten fg-alt 0.25))
   (dotemacs-neotree-media-file-face :foreground (dotemacs-lighten fg-alt 0.25))
   (neo-expand-btn-face :foreground magenta)

   ;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground green)

   ;; term
   (term-color-blue :background cyan :foreground cyan)
   (term-color-cyan :background green :foreground green)
   (term-color-green :background olive :foreground olive)

   ;;;;;;;; Brackets ;;;;;;;;
   ;; Rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground red)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground red)
   ;; Bracket pairing
   ((show-paren-match &override) :foreground nil :background fg-alt :bold t)
   ((show-paren-mismatch &override) :foreground nil :background "red")

   ;;;;;;;; which-key ;;;;;;;;
   (which-func :foreground green)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground (dotemacs-lighten fg-alt 0.25))
   (which-key-local-map-description-face :foreground cyan)

   ;;;;;;;; Company ;;;;;;;;
   (company-preview-common :foreground green)
   (company-tooltip-common :foreground green)
   (company-tooltip-common-selection :foreground green)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background fg)
   (company-scrollbar-fg :background green)
   (company-tooltip-selection :background accent)
   (company-tooltip-mouse :background accent :foreground nil)

   ;;;;;;;; Misc ;;;;;;;;
   (+workspace-tab-selected-face :background dark-green :foreground "white")

   ;; Undo tree
   (undo-tree-visualizer-active-branch-face :foreground green)
   (undo-tree-visualizer-current-face :foreground yellow)

   ;; General UI
   (button :foreground green :underline t :bold t)

   ;; ediff
   (ediff-fine-diff-A :background (dotemacs-blend red bg 0.3) :weight 'bold)

   ;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,green)  :background base3)

   ;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)

   ;;;;;;;; Major mode faces ;;;;;;;;
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground keywords)

   ;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground cyan)
   (markdown-list-face :foreground red)
   (markdown-url-face :foreground red)
   (markdown-pre-face  :foreground green)
   (markdown-link-face :inherit 'bold :foreground cyan)
   (markdown-code-face :background (dotemacs-lighten base2 0.045))

   ;; org-mode
   (org-level-1 :foreground yellow :bold bold)
   (org-level-2 :inherit 'org-level-1 :foreground cyan)
   (org-level-3 :bold bold :foreground green)
   (org-level-4 :inherit 'org-level-3)
   (org-level-5 :inherit 'org-level-3)
   (org-level-6 :inherit 'org-level-3)
   (org-ellipsis :underline nil :background base2 :foreground orange)
   (org-tag :foreground yellow :bold nil)
   (org-quote :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow))

  ;; --- extra variables --------------------
  ;; ()
  )
;;; dotemacs-gruvbox-theme.el ends here
