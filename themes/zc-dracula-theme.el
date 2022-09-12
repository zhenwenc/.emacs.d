;;; Inspired by Doom Dracula Theme -*- no-byte-compile: t; -*-

(require 'doom-themes)

(def-doom-theme zc-dracula
  "An uppity theme inspired by Dracula."

  ;; name        default   256       16
  ((bg         '("#282a36" nil       nil            ))
   (bg-alt     '("#1E2029" nil       nil            ))
   (base0      '("#1E2029" "black"   "black"        ))
   (base1      '("#282a36" "#1e1e1e" "brightblack"  ))
   (base2      '("#373844" "#2e2e2e" "brightblack"  ))
   (base3      '("#44475a" "#262626" "brightblack"  ))
   (base4      '("#565761" "#3f3f3f" "brightblack"  ))
   (base5      '("#6272a4" "#525252" "brightblack"  ))
   (base6      '("#b6b6b2" "#6b6b6b" "brightblack"  ))
   (base7      '("#ccccc7" "#979797" "brightblack"  ))
   (base8      '("#f8f8f2" "#dfdfdf" "white"        ))
   (-fg        '("#f8f8f2" "#2d2d2d" "white"        ))
   (fg-alt     '("#e2e2dc" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#ff5555" "#ff6655" "red"          ))
   (orange     '("#ffb86c" "#dd8844" "brightred"    ))
   (green      '("#50fa7b" "#99bb66" "green"        ))
   (teal       '("#0189cc" "#44b9b1" "brightgreen"  ))
   (yellow     '("#f1fa8c" "#ECBE7B" "yellow"       ))
   (blue       '("#61bfff" "#51afef" "brightblue"   ))
   (dark-blue  '("#0189cc" "#2257A0" "blue"         ))
   (magenta    '("#ff79c6" "#c678dd" "magenta"      ))
   (violet     '("#bd93f9" "#a9a1e1" "brightmagenta"))
   (cyan       '("#8be9fd" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#8be9fd" "#5699AF" "cyan"         ))

   ;; override dracula color palette
   (fg             (if (display-graphic-p) (doom-darken -fg 0.1) "#bfbfbf"))

   ;; font-lock face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      cyan)
   (functions      magenta)
   (keywords       (if (display-graphic-p) dark-cyan blue))
   (methods        teal)
   (operators      violet)
   (type           blue)
   (strings        (doom-blend yellow bg 0.9))
   (variables      fg)
   (numbers        red)
   (region         (doom-blend blue base3 0.2))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (level1 blue)
   (level2 yellow)
   (level3 cyan)
   (level4 yellow)
   (level5 blue)
   (level6 orange)
   (level7 yellow)
   (level8 magenta)
   (level9 violet)

   (hidden          base1)

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg          `(,(car bg-alt) nil))
   (modeline-bg-inactive `(,(car bg-alt) nil))

   (modeline-text          "#d5d8dc")
   (modeline-text-inverse  "#3b4044")
   (modeline-text-inactive "#5b6268")
   (modeline-primary       "#4296ec")
   (modeline-accent        "#ffd203")
   (modeline-warning       "#ff0266")
   (modeline-normal        "#e6af3f")
   (modeline-insert        "#2ecc71")
   (modeline-motion        "#ee42f4")
   (modeline-visual        "#ecf0f1")
   (modeline-pad           3))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :inherit 'font-lock-comment-face :foreground doc-comments)

   ;; --- modeline faces ------------------------
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   (header-line :inherit 'mode-line)

   (zc-modeline/active)
   (zc-modeline/inactive :inherit 'mode-line-inactive)

   (zc-modeline/primary
    :inherit 'zc-modeline/active
    :foreground modeline-primary)
   (zc-modeline/accent
    :inherit 'zc-modeline/active
    :foreground modeline-accent)
   (zc-modeline/warning
    :inherit 'zc-modeline/active :weight 'bold
    :foreground modeline-warning)

   (zc-modeline/evil-active
    :inherit 'zc-modeline/active :weight 'bold
    :background modeline-bg :foreground modeline-text-inverse)
   (zc-modeline/evil-inactive
    :inherit 'zc-modeline/active :weight 'bold
    :background modeline-bg :foreground modeline-text-inactive
    :box `(:line-width ,modeline-pad :color ,modeline-bg :style nil))

   (zc-modeline/evil-visual-state
    :inherit 'zc-modeline/evil-active :background modeline-motion
    :box `(:line-width ,modeline-pad :color ,modeline-motion :style nil))
   (zc-modeline/evil-normal-state
    :inherit 'zc-modeline/evil-active :background modeline-normal
    :box `(:line-width ,modeline-pad :color ,modeline-normal :style nil))
   (zc-modeline/evil-insert-state
    :inherit 'zc-modeline/evil-active :background modeline-insert
    :box `(:line-width ,modeline-pad :color ,modeline-insert :style nil))
   (zc-modeline/evil-visual-state
    :inherit 'zc-modeline/evil-active :background modeline-visual
    :box `(:line-width ,modeline-pad :color ,modeline-visual :style nil))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-level-1 :background base1 :foreground level1 :height 1.2 :weight 'bold)
   (org-level-2 :foreground level2 :weight 'bold)
   (org-level-3 :inherit 'org-level-2 :foreground level3)
   (org-level-4 :inherit 'org-level-2 :foreground level4)
   (org-level-5 :inherit 'org-level-2 :foreground level5)
   (org-level-6 :inherit 'org-level-2 :foreground level6)
   (org-level-7 :inherit 'org-level-2 :foreground level7)
   (org-todo :foreground magenta :bold 'inherit :background (doom-darken base1 0.02))
   (org-done :foreground green :strike-through nil :background base2 :bold t)
   (org-headline-done :foreground base4 :strike-through nil)
   ((org-tag &override) :foreground (doom-lighten orange 0.3))
   (org-agenda-date :foreground cyan)
   (org-agenda-dimmed-todo-face :foreground comments)
   (org-agenda-done :foreground base4)
   (org-agenda-structure :foreground violet)
   (org-block            :background (doom-darken base1 0.125) :foreground violet)
   (org-block-begin-line :background (doom-darken base1 0.125) :foreground comments)
   (org-quote            :background (doom-darken base1 0.125) :foreground base7)
   (org-code :foreground yellow)
   (org-column :background base1)
   (org-column-title :background base1 :bold t :underline t)
   (org-date :foreground cyan)
   (org-document-info :foreground blue)
   (org-document-info-keyword :foreground comments)
   (org-ellipsis :foreground comments)
   (org-footnote :foreground blue)
   (org-headline-base :foreground comments :strike-through t :bold nil)
   (org-link :foreground violet :underline t :weight 'normal)
   (org-priority :foreground cyan)
   (org-scheduled :foreground green)
   (org-scheduled-previously :foreground yellow)
   (org-scheduled-today :foreground orange)
   (org-sexp-date :foreground base4)
   (org-special-keyword :foreground yellow)
   (org-table :foreground violet)
   (org-upcoming-deadline :foreground yellow)
   (org-warning :foreground magenta)

   ;; override doom-theme-common
   (font-lock-preprocessor-face :foreground operators)
   (ivy-virtual :foreground base7)

   ;; highlight-thing
   (highlight-thing :background (doom-blend yellow base3 0.1)
                    :foreground (if (display-graphic-p) nil "#FFF")
                    :inherit bold)

   ;; Darken markdown code block background
   (markdown-code-face :background bg-alt)

   ;; eshell
   (zc-eshell/prompt-pwd :foreground violet)
   (zc-eshell/prompt-git-branch :foreground cyan)
   ))
