;; dotemacs-sourcerer-theme.el --- a more Sourcerer version of dotemacs-one
;;; Commentary:
(require 'dotemacs-themes)
;;; Code:
;;
(defgroup dotemacs-sourcerer-theme nil
  "Options for dotemacs-themes"
  :group 'dotemacs-themes)

(defcustom dotemacs-sourcerer-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'dotemacs-sourcerer-theme
  :type 'boolean)

(defcustom dotemacs-sourcerer-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'dotemacs-sourcerer-theme
  :type 'boolean)

(defcustom dotemacs-sourcerer-comment-bg dotemacs-sourcerer-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'dotemacs-sourcerer-theme
  :type 'boolean)

(defcustom dotemacs-sourcerer-padded-modeline dotemacs-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'dotemacs-sourcerer-theme
  :type '(or integer boolean))


;;
(def-dotemacs-theme dotemacs-sourcerer
  "A dark theme based off of xero's Sourcerer VIM colorscheme"

  ((bg         '("#202020"))
   (bg-alt     '("#222222"))
   (base0      '("#1d2127"))
   (base1      '("#1d2127"))
   (base2      '("#272727"))
   (base3      '("#32353f"))
   (base4      '("#494952"))
   (base5      '("#62686E"))
   (base6      '("#757B80"))
   (base7      '("#9ca0a4"))
   (base8      '("#fdfdd5"))
   (fg         '("#c2c2b0"))
   (fg-alt     '("#5D656B"))

   (grey       base4)
   (red        '("#aa4450"))
   (orange     '("#dd8844"))
   (green      '("#858253"))
   (green-br   '("#99bb66"))
   (teal       '("#5b8583" "#44b9b1" ))
   (yellow     '("#d0770f"           ))
   (blue       '("#86aed5"           ))
   (dark-blue  '("#6688aa"           ))
   (magenta    '("#8686ae"           ))
   (violet     '("#8686ae"           ))
   (cyan       '("#5b8583"           ))
   (dark-cyan  '("#9aaccb"           ))
   ;; face categories
   (highlight      orange)
   (vertical-bar   base0)
   (selection      base5)
   (builtin        blue)
   (comments       (if dotemacs-sourcerer-brighter-comments dark-cyan "#5c5d56"))
   (doc-comments   (if dotemacs-sourcerer-brighter-comments (dotemacs-lighten dark-cyan 0.15) (dotemacs-darken "#5c5d56" 0.1)))
   (constants      teal)
   (functions      base8)
   (keywords       blue)
   (methods        magenta)
   (operators      teal)
   (type           violet)
   (strings        green)
   (variables      base8)
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)
   
   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when dotemacs-sourcerer-padded-modeline
      (if (integerp dotemacs-sourcerer-padded-modeline) dotemacs-sourcerer-padded-modeline 4)))

   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (dotemacs-blend blue grey (if dotemacs-sourcerer-brighter-modeline 0.4 0.08)))
   
   (modeline-bg
    (if dotemacs-sourcerer-brighter-modeline
        `("#383f58" ,@(cdr base1))
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-l
    (if dotemacs-sourcerer-brighter-modeline
        modeline-bg
      `(,(dotemacs-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (dotemacs-darken bg 0.20))
   (modeline-bg-inactive-l `(,(dotemacs-darken (car bg-alt) 0.2) ,@(cdr base0))))
  
  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   (cursor :background blue)
   (font-lock-comment-face
    :foreground comments
    :background (if dotemacs-sourcerer-comment-bg (dotemacs-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (mode-line-buffer-id :foreground green-br :bold bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)
   
   (dotemacs-modeline-bar :background (if dotemacs-sourcerer-brighter-modeline modeline-bg highlight))
   (dotemacs-modeline-buffer-path :foreground (if dotemacs-sourcerer-brighter-modeline base8 blue) :bold bold)

   (mode-line
    :background base3 :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base3)))
   (mode-line-inactive
    :background bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if dotemacs-sourcerer-brighter-modeline base8 highlight))
   (fringe :background base2)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)
      ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground dark-cyan)
   (rainbow-delimiters-depth-2-face :foreground teal)
   (rainbow-delimiters-depth-3-face :foreground dark-blue)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground orange)
   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden-alt))


  ;; --- extra variables --------------------
  ;; ()

  )

;;; dotemacs-sourcerer-theme.el ends here
