;;; tokyonight-themes.el --- TokyoNight themes. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defgroup tokyonight-themes ()
  "TokyoNight themes."
  :group 'faces)

(defconst tokyonight-themes-faces
  '(
    ;; Basic faces
    `(cursor ((,c :background ,fg)))
    `(default ((,c :foreground ,fg :background ,bg)))
    `(fringe ((,c :foreground ,fg-dark :background ,bg-dark)))
    `(menu ((,c :background ,bg-dark :foreground ,fg)))
    `(scroll-bar ((,c :background ,bg-dark :foreground ,fg-dark)))
    `(tool-bar ((,c :background ,bg-dark :foreground ,fg)))
    `(button ((,c :background ,bg :foreground ,blue1 :underline ,blue1)))
    `(link ((,c :inherit button)))
    `(link-visited ((,c :background ,bg :foreground ,magenta :underline ,magenta)))
    `(match ((,c :foreground ,blue :background ,bg :inverse-video t)))
    `(shadow ((,c :foreground ,dark5)))
    `(minibuffer-prompt ((,c :foreground ,magenta :background unspecified)))
    `(region ((,c :background ,dark3 :foreground ,fg :extend t)))
    `(secondary-selection ((,c :background ,bg-hl :foreground unspecified)))
    `(trailing-whitespace ((,c :foreground ,white :background ,red)))
    `(border ((,c :background ,bg :foreground ,fg-gutter)))
    `(vertical-border ((,c :foreground ,fg-gutter)))
    `(tooltip ((,c :background ,bg-hl :foreground ,fg)))
    `(highlight ((,c :background ,bg-hl)))
    `(error ((,c :foreground ,red)))
    `(warning ((,c :foreground ,orange)))
    `(success ((,c :foreground ,green)))

    ;; line-number
    `(line-number ((,c :inherit default :background ,bg-dark :foreground ,fg-dark)))
    `(line-number-current-line ((,c :inherit (bold line-number) :background ,fg-gutter :foreground ,fg)))
    `(line-number-major-tick ((,c :inherit line-number :foreground ,red)))
    `(line-number-minor-tick ((,c :inherit line-number :foreground ,fg-dark)))

    ;; isearch
    `(isearch ((,c :foreground ,yellow :background ,bg :inverse-video t)))
    `(isearch-fail ((,c :foreground ,red :background ,bg :inverse-video t)))
    `(isearch-group-1 ((,c :foreground ,blue1 :background ,bg :inverse-video t)))
    `(isearch-group-2 ((,c :foreground ,green1 :background ,bg :inverse-video t)))
    `(lazy-highlight ((,c :foreground ,cyan :background ,bg :inverse-video t)))

    `(font-lock-bracket-face ((,c :foreground ,blue1)))
    `(font-lock-builtin-face ((,c :foreground ,purple)))
    `(font-lock-comment-face ((,c :foreground ,comment)))
    `(font-lock-comment-delimiter-face ((,c :foreground ,comment)))
    `(font-lock-constant-face ((,c :foreground ,orange)))
    `(font-lock-doc-face ((,c :foreground ,green2)))
    `(font-lock-function-name-face ((,c :foreground ,blue)))
    `(font-lock-keyword-face ((,c :foreground ,magenta)))
    `(font-lock-negation-char-face ((,c :inherit error)))
    `(font-lock-operator-face ((,c :foreground ,magenta)))
    `(font-lock-preprocessor-face ((,c :foreground ,purple)))
    `(font-lock-regexp-grouping-backslash ((,c :foreground ,yellow)))
    `(font-lock-regexp-grouping-construct ((,c :foreground ,magenta)))
    `(font-lock-string-face ((,c :foreground ,green1)))
    `(font-lock-type-face ((,c :foreground ,blue)))
    `(font-lock-variable-name-face ((,c :foreground ,blue2)))
    `(font-lock-warning-face ((,c :foreground ,yellow)))

    ;; Calendar
    `(holiday-face ((,c :foreground ,red1)))

    ;; Compilation
    `(compilation-info ((,c :foreground ,green :weight bold)))
    `(compilation-warning ((,c :foreground ,orange :weight bold)))
    `(compilation-error ((,c :foreground ,red :weight bold)))
    `(compilation-line-number ((,c :inherit shadow)))
    `(compilation-column-number ((,c :inherit compilation-line-number)))
    `(compilation-mode-line-exit ((,c :foreground ,green :weight bold)))
    `(compilation-mode-line-fail ((,c :foreground ,red :weight bold)))
    `(compilation-mode-line-run ((,c :foreground ,orange :weight bold)))

    ;; completions
    `(completions-annotations ((,c :foreground ,comment :background unspecified :slant italic)))
    `(completions-common-part ((,c :foreground ,blue6 :background unspecified)))
    `(completions-first-difference ((,c :foreground ,orange :background unspecified :weight bold)))
    `(completions-highlight ((,c :foreground ,fg :background ,bg-hl)))
    `(completions-group-title ((,c :foreground ,dark5 :background unspecified :slant italic)))
    `(completions-group-separator ((,c :foreground ,dark5 :background unspecified :strike-through t)))

    ;; corfu
    `(corfu-default ((,c :foreground ,fg :background ,bg-dark)))
    `(corfu-current ((,c :foreground ,fg :background ,bg-hl :weight bold)))
    `(corfu-bar ((,c :foreground ,fg :background ,dark3)))
    `(corfu-border ((,c :foreground ,fg :background ,dark5)))

    ;; Custom
    `(custom-state ((,c :foreground ,green)))

    ;; diff
    `(diff-added ((,c :foreground ,green :background ,bg-hl)))
    `(diff-changed ((,c :foreground ,yellow :background ,bg-hl)))
    `(diff-changed-unspecified ((,c :inherit diff-changed)))
    `(diff-removed ((,c :foreground ,red :background ,bg-hl)))
    `(diff-indicator-added ((,c :inherit diff-added)))
    `(diff-indicator-changed ((,c :inherit diff-changed)))
    `(diff-indicator-removed ((,c :inherit diff-removed)))
    `(diff-refine-added ((,c :inherit diff-added :inverse-video t)))
    `(diff-refine-changed ((,c :inherit diff-changed :inverse-video t)))
    `(diff-refine-removed ((,c :inherit diff-removed :inverse-video t)))
    `(diff-context (()))
    `(diff-error ((,c :inherit error)))
    `(diff-file-header ((,c :inherit bold)))
    `(diff-function ((,c :background ,dark3)))
    `(diff-header (()))
    `(diff-hunk-header ((,c :inherit bold :background ,dark3)))
    `(diff-index ((,c :slant italic)))
    `(diff-nonexistent ((,c :inherit bold)))

    ;; diff-hl
    `(diff-hl-change ((,c :foreground ,bg :background ,yellow)))
    `(diff-hl-delete ((,c :foreground ,bg :background ,red)))
    `(diff-hl-insert ((,c :foreground ,bg :background ,green)))
    `(diff-hl-reverted-hunk-highlight ((,c :foreground ,bg :background ,fg)))

    ;; dired
    `(dired-broken-symlink ((,c :inherit button :foreground ,red)))
    `(dired-directory ((,c :foreground ,blue1)))
    `(dired-flagged ((,c :foreground ,red :weight bold :inverse-video t)))
    `(dired-header ((,c :inherit bold)))
    `(dired-ignored ((,c :inherit shadow)))
    `(dired-mark ((,c :inherit bold)))
    `(dired-marked ((,c :foreground ,cyan :weight bold :inverse-video t)))
    `(dired-perm-write ((,c :inherit shadow)))
    `(dired-symlink ((,c :foreground ,cyan :background ,bg :underline ,cyan)))
    `(dired-warning ((,c :inherit warning)))

    ;; eldoc-box
    `(eldoc-box-body ((,c :inherit tooltip)))
    `(eldoc-box-border ((,c :foreground ,fg :background ,dark5)))

    ;; eshell
    `(eshell-prompt ((,c :foreground ,magenta :weight bold)))
    `(eshell-ls-archive ((,c :foreground ,red)))
    `(eshell-ls-backup ((,c :inherit font-lock-comment-face)))
    `(eshell-ls-clutter ((,c :inherit font-lock-comment-face)))
    `(eshell-ls-directory ((,c :foreground ,blue)))
    `(eshell-ls-executable ((,c :foreground ,green)))
    `(eshell-ls-missing ((,c :inherit font-lock-warning-face)))
    `(eshell-ls-product ((,c :inherit font-lock-doc-face)))
    `(eshell-ls-special ((,c :foreground ,yellow :weight bold)))
    `(eshell-ls-symlink ((,c :foreground ,cyan :weight bold)))
    `(eshell-ls-unreadable ((,c :foreground ,fg)))

    ;; flymake
    `(flymake-error ((,c :underline (:style wave :color ,red))))
    `(flymake-warning ((,c :underline (:style wave :color ,orange))))
    `(flymake-note ((,c :underline (:style wave :color ,cyan))))

    ;; flyspell
    `(flyspell-duplicate ((,c :underline (:style wave :color ,orange))))
    `(flyspell-incorrect ((,c :underline (:style wave :color ,red))))

    ;; hl-line
    `(hl-line ((,c :background ,bg-hl :extend t)))

    ;; Add ido support
    `(ido-first-match ((,c :foreground ,yellow :bold nil)))
    `(ido-only-match ((,c :foreground ,orange)))
    `(ido-subdir ((,c :foreground ,green)))

    ;; mode-line / header-line
    `(mode-line ((,c :foreground ,fg :background ,bg-dark :weight normal :box (:line-width 1 :color ,bg-dark))))
    `(mode-line-buffer-id ((,c :foreground ,fg :background unspecified)))
    `(mode-line-active ((,c :inherit mode-line)))
    `(mode-line-inactive ((,c :inherit mode-line :foreground ,dark5 :background ,bg :weight normal)))
    `(mode-line-emphasis ((,c :foreground ,blue)))
    `(mode-line-highlight ((,c :foreground ,fg-gutter :background ,blue :box nil)))
    `(header-line ((,c :inherit mode-line)))
    `(header-line-highlight ((,c :inherit mode-line-highlight)))

    ;; multiple-cursors
    `(mc/cursor-bar-face ((,c :foreground ,fg :background ,bg :height 1)))
    `(mc/cursor-face ((,c :inverse-video t)))
    `(mc/region-face ((,c :inherit region)))

    ;; symbol-overlay
    `(symbol-overlay-default-face ((,c :background ,blue7)))
    `(symbol-overlay-face-1 ((,c :background ,blue :foreground "black")))
    `(symbol-overlay-face-2 ((,c :background ,magenta :foreground "black")))
    `(symbol-overlay-face-3 ((,c :background ,yellow :foreground "black")))
    `(symbol-overlay-face-4 ((,c :background ,magenta2 :foreground "black")))
    `(symbol-overlay-face-5 ((,c :background ,red :foreground "black")))
    `(symbol-overlay-face-6 ((,c :background ,orange :foreground "black")))
    `(symbol-overlay-face-7 ((,c :background ,green :foreground "black")))
    `(symbol-overlay-face-8 ((,c :background ,cyan :foreground "black")))

    ;; tab-line / tab-bar
    `(tab-line ((,c :foreground ,fg-dark :background ,bg-dark)))
    `(tab-line-tab ((,c :foreground ,fg :background ,bg)))
    `(tab-line-tab-inactive ((,c :foreground ,fg-dark :background ,bg-dark)))
    `(tab-line-tab-inactive-alternate ((,c :inherit tab-line-tab-inactive)))
    `(tab-line-tab-current ((,c :foreground ,fg :background ,bg)))
    `(tab-bar ((,c :foreground ,fg-dark :background ,bg-dark)))
    `(tab-bar-tab ((,c :foreground ,fg :background ,bg)))
    `(tab-bar-tab-inactive ((,c :foreground ,fg-dark :background ,bg-dark)))

    ;; vertico
    `(vertico-current ((,c :background ,bg-hl :weight bold)))

    ;; vundo
    `(vundo-default ((,c :inherit shadow)))
    `(vundo-highlight ((,c :inherit (bold vundo-node) :foreground ,red)))
    `(vundo-last-saved ((,c :inherit (bold vundo-node) :foreground ,blue)))
    `(vundo-saved ((,c :inherit vundo-mode :foreground ,blue5)))
  )
  "Face specs for use with `tokyonight-themes-theme'.")

(defconst tokyonight-themes-custom-variables
  '(
    `(ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
  )
  "Custom variables for `tokyonight-themes-theme'.")

;;; Theme macros

;;;; Instantiate a Modus theme

;;;###autoload
(defmacro tokyonight-themes-theme (name palette &optional overrides)
  "Bind NAME's color PALETTE. Optional OVERRIDES are appended to PALETTE."
  (declare (indent 0))
  `(let* ((c '((class color) (min-colors 256)))
          ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                    (append (symbol-value palette) (symbol-value overrides))))
     (custom-theme-set-faces ',name ,@tokyonight-themes-faces)
     (custom-theme-set-variables ',name ,@tokyonight-themes-custom-variables)))

;;;; Use theme colors

(defmacro tokyonight-themes-with-colors (&rest body)
  "Evaluate BODY with colors from current palette bound."
  (declare (indent 0))
  (let* ((theme (or (car (seq-filter (lambda (th)
                                      (string-prefix-p "tokyonight-" (symbol-name th)))
                                     custom-enabled-themes))
                    (user-error "No enabled tokyonight theme could be found"))))
    `(let* ((c '((class color) (min-colors 256)))
            ,@(mapcar (lambda (cons)
                        (list (car cons) (cdr cons)))
                      (append (symbol-value (intern (format "%s-palette" theme)))
                              (symbol-value (intern (format "%s-palette-overrides" theme))))))
      ,@body)))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'tokyonight-themes)
;;; tokyonight-themes.el ends here