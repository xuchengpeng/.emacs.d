;;; dotemacs-opera-theme.el --- Opera theme

(require 'dotemacs-themes)

(defgroup dotemacs-opera-theme nil
  "Options for dotemacs-themes"
  :group 'dotemacs-themes)

(defcustom dotemacs-opera-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'dotemacs-opera-theme
  :type 'boolean)

(defcustom dotemacs-opera-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'dotemacs-opera-theme
  :type 'boolean)

(defcustom dotemacs-opera-comment-bg dotemacs-opera-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'dotemacs-opera-theme
  :type 'boolean)

(defcustom dotemacs-opera-padded-modeline dotemacs-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'dotemacs-opera-theme
  :type '(choice integer boolean))

(defcustom dotemacs-opera-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'dotemacs-opera-theme
  :type 'symbol)

(def-dotemacs-theme dotemacs-opera
  "A dark Opera theme."

  ;; name        default   256       16
  ((bg         '("#323334" nil       nil            ))
   (bg-alt     '("#222224" nil       nil            ))
   (base0      '("#000000" "black"   "black"        ))
   (base1      '("#1e1e1e" "#1e1e1e" "brightblack"  ))
   (base2      '("#2e2e2e" "#2e2e2e" "brightblack"  ))
   (base3      '("#262626" "#262626" "brightblack"  ))
   (base4      '("#3f3f3f" "#3f3f3f" "brightblack"  ))
   (base5      '("#525252" "#525252" "brightblack"  ))
   (base6      '("#6b6b6b" "#6b6b6b" "brightblack"  ))
   (base7      '("#979797" "#979797" "brightblack"  ))
   (base8      '("#dfdfdf" "#dfdfdf" "white"        ))
   (fg         '("#eceff4" "#dfdfdf" "white"        ))
   (fg-alt     '("#727269" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#C16069" "#ff6655" "red"          ))
   (orange     '("#D2876D" "#dd8844" "brightred"    ))
   (green      '("#A2BF8A" "#99bb66" "green"        ))
   (teal       '("#8EBCBB" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECCC87" "#ECBE7B" "yellow"       ))
   (blue       '("#80A0C2" "#51afef" "brightblue"   ))
   (dark-blue  '("#5C748E" "#2257A0" "blue"         ))
   (magenta    '("#B58DAE" "#c678dd" "magenta"      ))
   (violet     '("#5D80AE" "#a9a1e1" "brightmagenta"))
   (cyan       '("#86C0D1" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#507681" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (dotemacs-darken base1 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       (if dotemacs-opera-brighter-comments dark-cyan (dotemacs-lighten base5 0.2)))
   (doc-comments   (dotemacs-lighten (if dotemacs-opera-brighter-comments dark-cyan base5) 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (dotemacs-lighten magenta 0.5))
   (numbers        magenta)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright dotemacs-opera-brighter-modeline)
   (-modeline-pad
    (when dotemacs-opera-padded-modeline
      (if (integerp dotemacs-opera-padded-modeline) dotemacs-opera-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (dotemacs-darken blue 0.475)
      `(,(dotemacs-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (dotemacs-darken blue 0.45)
      `(,(dotemacs-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (dotemacs-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  (
   ((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if dotemacs-opera-comment-bg (dotemacs-lighten bg 0.05)))
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
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; dotemacs-opera-theme.el ends here
