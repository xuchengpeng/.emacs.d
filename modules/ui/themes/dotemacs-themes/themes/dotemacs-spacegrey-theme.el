;;; dotemacs-spacegrey-theme.el --- inspired by Atom Spacegrey Dark
(require 'dotemacs-themes)

(defgroup dotemacs-spacegrey-theme nil
  "Options for dotemacs-themes"
  :group 'dotemacs-themes)

(defcustom dotemacs-spacegrey-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'dotemacs-spacegrey-theme
  :type 'boolean)

(defcustom dotemacs-spacegrey-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'dotemacs-spacegrey-theme
  :type 'boolean)

(defcustom dotemacs-spacegrey-comment-bg dotemacs-spacegrey-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'dotemacs-spacegrey-theme
  :type 'boolean)

(defcustom dotemacs-spacegrey-padded-modeline dotemacs-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'dotemacs-spacegrey-theme
  :type '(choice integer boolean))

;;
(def-dotemacs-theme dotemacs-spacegrey
  "A dark theme inspired by Atom Spacegrey Dark"

  ;; name        default   256       16
  ((bg         '("#2b303b" nil       nil            ))
   (bg-alt     '("#232830" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#2F3237" "#2F3237" "brightblack"  ))
   (base4      '("#4f5b66" "#4f5b66" "brightblack"  ))
   (base5      '("#65737E" "#65737E" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#c0c5ce" "#c0c5ce" "brightwhite"  ))
   (fg-alt     '("#c0c5ce" "#c0c5ce" "white"        ))

   (grey       base4)
   (red        '("#BF616A" "#BF616A" "red"          ))
   (orange     '("#D08770" "#D08770" "brightred"    ))
   (green      '("#A3BE8C" "#A3BE8C" "green"        ))
   (blue       '("#8FA1B3" "#8FA1B3" "brightblue"   ))
   (violet     '("#b48ead" "#b48ead" "brightmagenta"))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      orange)
   (vertical-bar   (dotemacs-darken bg 0.25))
   (selection      base4)
   (builtin        orange)
   (comments       base5)
   (doc-comments   (dotemacs-lighten (if dotemacs-spacegrey-brighter-comments dark-cyan base5) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright dotemacs-spacegrey-brighter-modeline)
   (-modeline-pad
    (when dotemacs-spacegrey-padded-modeline
      (if (integerp dotemacs-spacegrey-padded-modeline) dotemacs-spacegrey-padded-modeline 4)))



   ;; --- Modeline config -------------------

   (modeline-fg     nil)
   (modeline-fg-alt (dotemacs-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (dotemacs-darken base3 0.05)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (dotemacs-darken base3 0.1)
      base1))
   (modeline-bg-inactive (dotemacs-darken bg 0.1))
   (modeline-bg-inactive-l `(,(dotemacs-darken (car bg-alt) 0.05) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if dotemacs-spacegrey-comment-bg (dotemacs-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (dotemacs-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

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
   (css-property             :foreground fg)
   (css-selector             :foreground red)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (dotemacs-darken bg 0.1))

   ;; org-mode
   (org-block            :background (dotemacs-darken bg-alt 0.04))
   (org-block-begin-line :foreground base4 :slant 'italic :background (dotemacs-darken bg 0.04))

   (org-level-1 :foreground fg   :weight 'ultra-bold :inherit 'hl-line :height 1.2)
   (org-level-2 :foreground (dotemacs-blend fg blue 0.35) :weight 'bold)
   (org-level-3 :foreground (dotemacs-blend fg blue 0.7)  :weight 'bold)
   (org-level-4 :foreground blue       :weight 'bold)
   (org-level-5 :foreground (dotemacs-blend magenta blue 0.2) :weight 'bold)
   (org-level-6 :foreground (dotemacs-blend magenta blue 0.4) :weight 'bold)
   (org-level-7 :foreground (dotemacs-blend magenta blue 0.6) :weight 'bold)
   (org-level-8 :foreground fg :weight 'semi-bold)

   (org-ellipsis         :underline nil :background bg    :foreground red)
   (org-quote            :background base1)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   (tooltip              :background bg-alt :foreground fg))

  ;; --- extra variables ---------------------
  ;; ()
  )

;;; dotemacs-spacegrey-theme.el ends here