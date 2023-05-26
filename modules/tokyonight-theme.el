;;; tokyonight-theme.el --- TokyoNight theme. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(deftheme tokyonight
  "TokyoNight theme.")

(defgroup tokyonight-theme nil
  "TokyoNight theme group."
  :group 'faces)

(defcustom tokyonight-theme-style 'storm
  "Default color style."
  :type 'choice
  :group 'tokyonight-theme)

(defcustom tokyonight-override-colors-alist '()
  "Place to override default theme colors."
  :type 'alist
  :group 'tokyonight-theme)

(defconst tokyonight-storm-colors-alist
  '(("bg-dark" . "#1f2335")
    ("bg" . "#24283b")
    ("bg-hl" . "#292e42")
    ("terminal-black" . "#414868")
    ("fg" . "#c0caf5")
    ("fg-dark" . "#a9b1d6")
    ("fg-gutter" . "#3b4261")
    ("white" . "#ffffff")
    ("comment" . "#565f89")
    ("dark3" . "#545c7e")
    ("dark5" . "#737aa2")
    ("blue0" . "#3d59a1")
    ("blue" . "#7aa2f7")
    ("blue1" . "#2ac3de")
    ("blue2" . "#0db9d7")
    ("blue5" . "#89ddff")
    ("blue6" . "#b4f9f8")
    ("blue7" . "#394b70")
    ("cyan" . "#7dcfff")
    ("magenta" . "#bb9af7")
    ("magenta2" . "#ff007c")
    ("purple" . "#9d7cd8")
    ("orange" . "#ff9e64")
    ("yellow" . "#e0af68")
    ("green" . "#9ece6a")
    ("green1" . "#73daca")
    ("green2" . "#41a6b5")
    ("teal" . "#1abc9c")
    ("red" . "#f7768e")
    ("red1" . "#db4b4b")))

(defconst tokyonight-night-colors-alist
  (append tokyonight-storm-colors-alist
          '(("bg" . "#1a1b26")
            ("bg-dark" . "#16161e"))))

(defconst tokyonight-moon-colors-alist
  '(("bg-dark" . "#1e2030")
    ("bg" . "#222436")
    ("bg-hl" . "#2f334d")
    ("terminal-black" . "#444a73")
    ("fg" . "#c8d3f5")
    ("fg-dark" . "#828bb8")
    ("fg-gutter" . "#3b4261")
    ("white" . "#ffffff")
    ("comment" . "#7a88cf")
    ("dark3" . "#545c7e")
    ("dark5" . "#737aa2")
    ("blue0" . "#3e68d7")
    ("blue" . "#82aaff")
    ("blue1" . "#65bcff")
    ("blue2" . "#0db9d7")
    ("blue5" . "#89ddff")
    ("blue6" . "#b4f9f8")
    ("blue7" . "#394b70")
    ("cyan" . "#86e1fc")
    ("magenta" . "#c099ff")
    ("magenta2" . "#ff007c")
    ("purple" . "#fca7ea")
    ("orange" . "#ff966c")
    ("yellow" . "#ffc777")
    ("green" . "#c3e88d")
    ("green1" . "#4fd6be")
    ("green2" . "#41a6b5")
    ("teal" . "#4fd6be")
    ("red" . "#ff757f")
    ("red1" . "#c53b53")))

(defconst tokyonight-day-colors-alist
  '(("bg-dark" . "#e9e9ec")
    ("bg" . "#e1e2e7")
    ("bg-hl" . "#c4c8da")
    ("terminal-black" . "#a1a6c5")
    ("fg" . "#3760bf")
    ("fg-dark" . "#6172b0")
    ("fg-gutter" . "#a8aecb")
    ("white" . "#ffffff")
    ("comment" . "#848cb5")
    ("dark3" . "#8990b3")
    ("dark5" . "#68709a")
    ("blue0" . "#7890dd")
    ("blue" . "#2e7de9")
    ("blue1" . "#188092")
    ("blue2" . "#07879d")
    ("blue5" . "#006a83")
    ("blue6" . "#2e5857")
    ("blue7" . "#92a6d5")
    ("cyan" . "#007197")
    ("magenta" . "#9854f1")
    ("magenta2" . "#d20065")
    ("purple" . "#7847bd")
    ("orange" . "#b15c00")
    ("yellow" . "#8c6c3e")
    ("green" . "#587539")
    ("green1" . "#387068")
    ("green2" . "#38919f")
    ("teal" . "#118c74")
    ("red" . "#f52a65")
    ("red1" . "#c64343")))

(defmacro tokyonight-with-color-variables (&rest body)
  "Execute BODY with variables bound to the colors."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (cond
                    ((eq tokyonight-theme-style 'storm)
                     (append tokyonight-storm-colors-alist tokyonight-override-colors-alist))
                    ((eq tokyonight-theme-style 'night)
                     (append tokyonight-night-colors-alist tokyonight-override-colors-alist))
                    ((eq tokyonight-theme-style 'moon)
                     (append tokyonight-moon-colors-alist tokyonight-override-colors-alist))
                    ((eq tokyonight-theme-style 'day)
                     (append tokyonight-day-colors-alist tokyonight-override-colors-alist))
                    (t
                     (append tokyonight-storm-colors-alist tokyonight-override-colors-alist)))))
     ,@body))

(tokyonight-with-color-variables
  (custom-theme-set-faces
   'tokyonight

   ;; Basic faces
   `(cursor ((t (:background ,white))))
   `(default ((t ,(list :foreground fg :background bg))))
   `(fringe ((t ,(list :foreground fg-dark :background bg-dark))))
   `(link ((t (:foreground ,dark5 :underline t))))
   `(link-visited ((t (:foreground ,dark5 :underline t))))
   `(match ((t (:foreground ,orange :background ,bg :inverse-video t))))
   `(shadow ((t (:foreground ,dark5))))
   `(minibuffer-prompt ((t :foreground ,magenta :background unspecified)))
   `(region ((t (:background ,bg-hl :extend t))))
   `(secondary-selection ((t ,(list :background bg-hl :foreground 'unspecified))))
   `(trailing-whitespace ((t ,(list :foreground white :background red))))
   `(border ((t ,(list :background bg :foreground bg-hl))))
   `(vertical-border ((t (:foreground ,fg-gutter))))
   `(tooltip ((t ,(list :background bg-hl :foreground fg))))
   `(highlight ((t (:background ,bg-hl))))
   `(error ((t (:foreground ,red))))
   `(warning ((t (:foreground ,orange))))
   `(success ((t (:foreground ,green))))

   ;; line-number
   `(line-number ((t (:inherit default :foreground ,dark3))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,fg))))

   ;; isearch
   `(isearch ((t (:foreground ,orange :background ,bg :inverse-video t))))
   `(isearch-fail ((t (:foreground ,orange :background ,red1 :inverse-video t))))
   `(lazy-highlight ((t (:foreground ,cyan :background ,bg :inverse-video t))))

   `(font-lock-bracket-face ((t (:foreground ,blue1))))
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,orange))))
   `(font-lock-doc-face ((t (:foreground ,green))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,magenta))))
   `(font-lock-negation-char-face ((t (:foreground ,green))))
   `(font-lock-operator-face ((t (:foreground ,magenta))))
   `(font-lock-preprocessor-face ((t (:foreground ,purple))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,magenta))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,yellow))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,red1))))

   ;; centaur-tabs
   `(centaur-tabs-default ((t (:inherit tab-bar :box nil))))
   `(centaur-tabs-selected ((t (:inherit tab-bar-tab :box nil :weight bold))))
   `(centaur-tabs-unselected ((t (:inherit tab-bar-tab-inactive :box nil :weight light))))
   `(centaur-tabs-selected-modified ((t (:inherit centaur-tabs-selected :foreground ,orange))))
   `(centaur-tabs-unselected-modified ((t (:inherit centaur-tabs-unselected :foreground ,orange))))
   `(centaur-tabs-active-bar-face ((t (:inherit mode-line-highlight))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected :foreground ,orange))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected :foreground ,orange))))

   ;; company
   `(company-tooltip ((t (:foreground ,fg :background ,bg-hl))))
   `(company-tooltip-selection ((t (:foreground ,comment :inverse-video t))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground ,red))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground ,red))))
   `(company-tooltip-search ((t (:inherit company-tooltip :foreground ,blue))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground ,green))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection :foreground ,green))))
   `(company-tooltip-scrollbar-thumb ((t (:background ,dark3))))
   `(company-tooltip-scrollbar-track ((t (:background ,bg-hl))))
   `(company-preview ((t (:foreground ,comment :background ,bg-hl))))
   `(company-preview-common ((t (:inherit company-preview :foreground ,red))))
   `(company-preview-search ((t (:inherit company-preview :foreground ,blue))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground green :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground yellow :bold t :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,red))))
   `(compilation-mode-line-failt ((t ,(list :foreground red :weight 'bold :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground green :weight 'bold :inherit 'unspecified))))

   ;; completions
   `(completions-annotations ((t (:foreground ,comment :background unspecified :slant italic))))
   `(completions-common-part ((t (:foreground ,blue6 :background unspecified))))
   `(completions-first-difference ((t (:foreground ,orange :background unspecified :weight bold))))
   `(completions-highlight ((t (:foreground ,fg :background ,bg-hl))))
   `(completions-group-title ((t (:foreground ,dark5 :background unspecified :slant italic))))
   `(completions-group-separator ((t (:foreground ,dark5 :background unspecified :strike-through t))))

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

   ;; eshell
   `(eshell-prompt ((t (:foreground ,magenta :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,red))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,fg))))

   ;; flycheck
   `(flycheck-error          ((t (:underline (:style wave :color ,red)))))
   `(flycheck-warning        ((t (:underline (:style wave :color ,orange)))))
   `(flycheck-info           ((t (:underline (:style wave :color ,green)))))
   `(flycheck-fringe-error   ((t (:inherit fringe :foreground ,red))))
   `(flycheck-fringe-warning ((t (:inherit fringe :foreground ,orange))))
   `(flycheck-fringe-info    ((t (:inherit fringe :foreground ,green))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:style wave :color ,orange) :inherit unspecified))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,red) :inherit unspecified))))

   ;; Add ido support
   `(ido-first-match ((t (:foreground ,yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,orange))))
   `(ido-subdir ((t (:foreground ,green))))

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

   ;; vertico
   `(vertico-current ((t (:inherit region :extend t))))
  )

  (custom-theme-set-variables
   'tokyonight
   `(ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg]))
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	             (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tokyonight)
;;; tokyonight-theme.el ends here
