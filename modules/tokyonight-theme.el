;;; tokyonight-theme.el --- TokyoNight theme.
;;; Commentary:
;;; Code:

(deftheme tokyonight
  "TokyoNight theme.")

(let ((bg-dark "#1e2030")
      (bg "#222436")
      (bg-hl "#2f334d")
      (terminal-black "#444a73")
      (fg "#c8d3f5")
      (fg-dark "#828bb8")
      (fg-gutter "#3b4261")
      (white "#ffffff")
      (comment "#7a88cf")
      (dark3 "#545c7e")
      (dark5 "#737aa2")
      (blue0 "#3e68d7")
      (blue "#82aaff")
      (blue1 "#65bcff")
      (blue2 "#0db9d7")
      (blue5 "#89ddff")
      (blue6 "#b4f9f8")
      (blue7 "#394b70")
      (cyan "#86e1fc")
      (magenta "#c099ff")
      (magenta2 "#ff007c")
      (purple "#fca7ea")
      (orange "#ff966c")
      (yellow "#ffc777")
      (green "#c3e88d")
      (green1 "#4fd6be")
      (green2 "#41a6b5")
      (teal "#4fd6be")
      (red "#ff757f")
      (red1 "#c53b53"))
  (custom-theme-set-variables
   'tokyonight
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'tokyonight

   ;; Basic faces
   `(cursor ((t (:background ,white))))
   `(default ((t ,(list :foreground fg :background bg))))
   `(fringe ((t ,(list :background bg-dark :foreground fg-gutter))))
   `(link ((t (:foreground ,dark5 :underline t))))
   `(link-visited ((t (:foreground ,dark5 :underline t))))
   `(match ((t (:background ,dark5))))
   `(shadow ((t (:foreground ,dark5))))
   `(minibuffer-prompt ((t :foreground ,magenta)))
   `(region ((t (:background ,bg-hl :foreground unspecified))))
   `(secondary-selection ((t ,(list :background bg-hl :foreground 'unspecified))))
   `(trailing-whitespace ((t ,(list :foreground white :background red))))
   `(border ((t ,(list :background bg :foreground bg-hl))))
   `(vertical-border ((t (:foreground ,fg-gutter))))
   `(tooltip ((t ,(list :background bg-hl :foreground fg))))
   `(highlight ((t (:background ,bg-hl))))
   `(error ((t (:foreground ,red))))
   `(warning ((t (:foreground ,orange))))
   `(success ((t (:foreground ,green))))
   `(line-number ((t (:inherit default :foreground ,comment :background ,bg))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,fg :background ,bg-hl))))

   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,orange))))
   `(font-lock-doc-face ((t (:foreground ,green))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,magenta))))
   `(font-lock-negation-char-face ((t (:foreground ,green))))
   `(font-lock-preprocessor-face ((t (:foreground ,purple))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,magenta))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,blue0))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,yellow))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,red1))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground green :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground yellow :bold t :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,red))))
   `(compilation-mode-line-failt ((t ,(list :foreground red :weight 'bold :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground green :weight 'bold :inherit 'unspecified))))

   ;; Custom
   `(custom-state ((t (:foreground ,green))))

   ;; Diff
   `(diff-changed ((t ,(list :foreground blue :background 'unspecified))))
   `(diff-removed ((t ,(list :foreground red :background 'unspecified))))
   `(diff-added ((t ,(list :foreground green :background 'unspecified))))

   ;; diff-hl
   `(diff-hl-change ((t (:foreground ,bg :background ,blue))))
   `(diff-hl-delete ((t (:foreground ,bg :background ,red))))
   `(diff-hl-insert ((t (:foreground ,bg :background ,green))))

   ;; Dired
   `(dired-directory ((t (:foreground ,magenta))))
   `(dired-ignored ((t ,(list :foreground blue7 :inherit 'unspecified))))

   ;; Add ido support
   `(ido-first-match ((t (:foreground ,yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,orange))))
   `(ido-subdir ((t (:foreground ,green))))

   ;; eshell
   `(eshell-ls-backup ((t (:foreground ,orange))))
   `(eshell-ls-directory ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-symlink ((t (:foreground ,yellow))))

   ;; mode-line / header-line
   `(mode-line ((t (:foreground ,fg :background ,bg-dark :weight normal :box (:line-width 1 :color ,bg-dark)))))
   `(mode-line-buffer-id ((t (:foreground ,fg :background unspecified))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:inherit mode-line :foreground ,dark5 :background ,bg :weight normal))))
   `(mode-line-emphasis ((t (:foreground ,blue))))
   `(mode-line-highlight ((t (:foreground ,fg-gutter :background ,blue :box nil))))
   `(header-line ((t (:inherit mode-line))))
   `(header-line-highlight ((t (:inherit mode-line-highlight))))

   ;; tab-line / tab-bar
   `(tab-line ((t (:foreground ,fg-dark :background ,bg-dark))))
   `(tab-line-tab ((t (:foreground ,fg :background ,bg))))
   `(tab-line-tab-inactive ((t (:foreground ,fg-dark :background ,bg-dark))))
   `(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive))))
   `(tab-line-tab-current ((t (:foreground ,fg :background ,bg))))
   `(tab-bar ((t (:foreground ,fg-dark :background ,bg-dark))))
   `(tab-bar-tab ((t (:foreground ,fg :background ,bg))))
   `(tab-bar-tab-inactive ((t (:foreground ,fg-dark :background ,bg-dark))))

   ;; centaur-tabs
   `(centaur-tabs-default ((t (:inherit tab-bar :box nil))))
   `(centaur-tabs-selected ((t (:inherit tab-bar-tab :box nil))))
   `(centaur-tabs-unselected ((t (:inherit tab-bar-tab-inactive :box nil))))
   `(centaur-tabs-selected-modified ((t (:inherit centaur-tabs-selecte :foreground ,orange))))
   `(centaur-tabs-unselected-modified ((t (:inherit centaur-tabs-unselecte :foreground ,orange))))
   `(centaur-tabs-active-bar-face ((t (:inherit mode-line-highlight))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected :foreground ,orange))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected :foreground ,orange))))

   ;; company
   `(company-tooltip ((t (:foreground ,fg :background ,bg-dark))))
   `(company-tooltip-selection ((t (:foreground ,comment :background ,bg-hl))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground ,red))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground ,red))))
   `(company-tooltip-search ((t (:inherit company-tooltip :foreground ,blue))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground ,green))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection :foreground ,green))))
   `(company-scrollbar-fg ((t (:background ,bg-hl))))
   `(company-scrollbar-bg ((t (:background ,bg-dark))))
   `(company-preview ((t (:foreground ,comment :background ,bg-hl))))
   `(company-preview-common ((t (:inherit company-preview :foreground ,red))))
   `(company-preview-search ((t (:inherit company-preview :foreground ,blue))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	             (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tokyonight)
;;; tokyonight-theme.el ends here