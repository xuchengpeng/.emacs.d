;;; dotemacs-solarized-light-theme.el --- inspired by Atom One Dark
(require 'dotemacs-themes)

;;
(defgroup dotemacs-solarized-light-theme nil
  "Options for dotemacs-themes"
  :group 'dotemacs-themes)

(defcustom dotemacs-solarized-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'dotemacs-solarized-light-theme
  :type 'boolean)

(defcustom dotemacs-solarized-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'dotemacs-solarized-light-theme
  :type 'boolean)

(defcustom dotemacs-solarized-light-comment-bg dotemacs-solarized-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'dotemacs-solarized-light-theme
  :type 'boolean)

(defcustom dotemacs-solarized-light-padded-modeline dotemacs-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'dotemacs-solarized-light-theme
  :type '(choice integer boolean))

;;
(def-dotemacs-theme dotemacs-solarized-light
  "A light theme inspired by Solarized light"

  ;; name        default   256       16
  ((bg         '("#FDF6E3" nil       nil            ))
   (bg-alt     '("#FFFBEA" nil       nil            ))
   (base0      '("#FFFBF0" "black"   "black"        ))
   (base1      '("#FCF8ED" "#1e1e1e" "brightblack"  ))
   (base2      '("#FCF7E8" "#2e2e2e" "brightblack"  ))
   (base3      '("#F2E6CE" "#262626" "brightblack"  ))
   (base4      '("#E1DBCD" "#3f3f3f" "brightblack"  ))
   (base5      '("#D6D6D6" "#525252" "brightblack"  ))
   (base6      '("#96A7A9" "#6b6b6b" "brightblack"  ))
   (base7      '("#788484" "#979797" "brightblack"  ))
   (base8      '("#626C6C" "#dfdfdf" "white"        ))
   (fg         '("#556b72" "#2d2d2d" "white"        ))
   (fg-alt     '("#7B8787" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#268bd2" "#51afef" "brightblue"   ))
   (dark-blue  '("#E1E3E5" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#D7DDD7" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if dotemacs-solarized-light-brighter-comments
                       (dotemacs-lighten teal 0.25)
                     base6))
   (doc-comments   teal)
   (constants      violet)
   (functions      magenta)
   (keywords       green)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         `(,(dotemacs-darken (car bg-alt) 0.1) ,@(dotemacs-darken (cdr base0) 0.1)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright dotemacs-solarized-light-brighter-modeline)
   (-modeline-pad
    (when dotemacs-solarized-light-padded-modeline
      (if (integerp dotemacs-solarized-light-padded-modeline) dotemacs-solarized-light-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (dotemacs-lighten bg 0.7)
      (dotemacs-lighten base3 0.2)))
   (modeline-bg-l
    (if -modeline-bright
        (dotemacs-lighten bg 0.7)
      (dotemacs-darken bg 0.05)))
   (modeline-bg-inactive   (dotemacs-darken bg 0.02))
   (modeline-bg-inactive-l (dotemacs-darken bg 0.025)))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (hl-line :background base3)

   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg :background region :weight 'bold)

   (org-block :background (dotemacs-blend yellow bg 0.04))
   (org-block-background :background (dotemacs-blend yellow bg 0.04))
   (org-block-begin-line :background (dotemacs-blend yellow bg 0.08))
   (org-block-end-line :background (dotemacs-blend yellow bg 0.08))
   (font-lock-comment-face
    :slant 'italic
    :foreground comments
    :background (if dotemacs-solarized-light-comment-bg (dotemacs-blend teal base0 0.07)))
   ((font-lock-doc-face &override) :foreground doc-comments)
   ((font-lock-type-face &override) :slant 'italic)
   ((font-lock-builtin-face &override) :slant 'italic)
   ((font-lock-function-name-face &override) :foreground type)


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
   (markdown-code-face :background (dotemacs-lighten base3 0.05))

   ;; ivy-mode
   (ivy-current-match :background (dotemacs-lighten yellow 0.65) :distant-foreground fg)
   (ivy-minibuffer-match-face-1
    :foreground comments
    :weight 'light)
   (ivy-minibuffer-match-face-2 :foreground magenta :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground green   :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground yellow  :background base3 :weight 'bold)
   (ivy-minibuffer-match-highlight :foreground violet :weight 'bold)

   ;; posframe
   (ivy-posframe :background modeline-bg-l)
   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; dotemacs-solarized-light-theme.el ends here
