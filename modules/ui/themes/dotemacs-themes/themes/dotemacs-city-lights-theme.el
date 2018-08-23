;;; dotemacs-city-lights-theme.el --- inspired by Atom City Lights
(require 'dotemacs-themes)

;;
(defgroup dotemacs-city-lights-theme nil
  "Options for dotemacs-themes"
  :group 'dotemacs-themes)

(defcustom dotemacs-city-lights-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'dotemacs-city-lights-theme
  :type 'boolean)

(defcustom dotemacs-city-lights-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'dotemacs-city-lights-theme
  :type 'boolean)

(defcustom dotemacs-city-lights-comment-bg dotemacs-city-lights-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'dotemacs-city-lights-theme
  :type 'boolean)

(defcustom dotemacs-city-lights-padded-modeline dotemacs-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'dotemacs-city-lights-theme
  :type '(or integer boolean))

;;
(def-dotemacs-theme dotemacs-city-lights
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#1D252C" nil       nil            ))
   (bg-alt     '("#181E24" nil       nil            ))
   (base0      '("#10151C" "black"   "black"        ))
   (base1      '("#171D22" "#1e1e1e" "brightblack"  ))
   (base2      '("#20282F" "#2e2e2e" "brightblack"  ))
   (base3      '("#28323B" "#262626" "brightblack"  ))
   (base4      '("#384551" "#3f3f3f" "brightblack"  ))
   (base5      '("#56697A" "#525252" "brightblack"  ))
   (base6      '("#688094" "#6b6b6b" "brightblack"  ))
   (base7      '("#7FA0B7" "#979797" "brightblack"  ))
   (base8      '("#9CAABB" "#dfdfdf" "white"        ))
   (fg-alt     '("#728CA0" "#bfbfbf" "brightwhite"  ))
   (fg         '("#A0B3C5" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#D95468" "#ff6655" "red"          ))
   (orange     '("#D98E48" "#dd8844" "brightred"    ))
   (green      '("#8BD49C" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#EBBF83" "#ECBE7B" "yellow"       ))
   (blue       '("#5EC4FF" "#51afef" "brightblue"   ))
   (dark-blue  '("#5C748E" "#2257A0" "blue"         ))
   (magenta    '("#E27E8D" "#c678dd" "magenta"      ))
   (violet     '("#B62D65" "#a9a1e1" "brightmagenta"))
   (cyan       '("#70E1E8" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#41858C" "#5699AF" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (dotemacs-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if dotemacs-city-lights-brighter-comments dark-cyan base5))
   (doc-comments   (dotemacs-lighten (if dotemacs-city-lights-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      teal)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        base7)
   (variables      base8)
   (numbers        magenta)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright dotemacs-city-lights-brighter-modeline)
   (-modeline-pad
    (when dotemacs-city-lights-padded-modeline
      (if (integerp dotemacs-city-lights-padded-modeline) dotemacs-city-lights-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(dotemacs-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(dotemacs-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (dotemacs-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if dotemacs-city-lights-comment-bg (dotemacs-lighten bg 0.05)))
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
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (dotemacs-darken blue 0.2))
   ((outline-5 &override) :foreground (dotemacs-darken green 0.2))
   ((outline-6 &override) :foreground (dotemacs-darken teal 0.2))
   ((outline-7 &override) :foreground (dotemacs-darken blue 0.4))
   ((outline-8 &override) :foreground (dotemacs-darken green 0.4))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; dotemacs-city-lights-theme.el ends here
