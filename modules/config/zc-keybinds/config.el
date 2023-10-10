;; Major mode leader key
(setq doom-localleader-key ",")


;; Global key bindings

(map!
 "s-a"         #'zc-core/evil-escape
 "s-s"         #'zc-core/evil-escape-and-save

 "s-0"         #'winum-select-window-0-or-10
 "s-1"         #'winum-select-window-1
 "s-2"         #'winum-select-window-2
 "s-3"         #'winum-select-window-3
 "s-4"         #'winum-select-window-4
 "s-5"         #'winum-select-window-5
 "s-6"         #'winum-select-window-6
 "s-7"         #'winum-select-window-7
 "s-8"         #'winum-select-window-8
 "s-9"         #'winum-select-window-9

 "s-y"         #'consult-yank-pop

 :nv  "C-i"    #'indent-for-tab-command
 :nv  "C-o"    #'goto-last-change
 :nv  "C-S-o"  #'goto-last-change-reverse

 :nv  "C-e"    #'evil-end-of-line
 :i   "C-e"    #'mwim-end-of-code-or-line

 :nv  "<tab>"  #'evil-indent-line

 :nv  "s-u"    #'evil-scroll-line-up
 :nv  "s-d"    #'evil-scroll-line-down

 :ni  "M-k"    #'kill-whole-line
 :ni  "s-."    #'xref-find-definitions
 :ni  "s-,"    #'xref-go-back
 :ni  "s->"    #'xref-find-definitions-other-window

 :ni  "s-;"    #'comment-dwim
 :n     ";"    #'evilnc-comment-operator

 :n "s-v"      #'evil-paste-after

 :n  "M"       #'evil-show-marks
 :n  "s"       #'evil-substitute
 :v  "S"       #'evil-substitute
 :v  "s"       #'evil-surround-region

 :i  "C-d"     #'delete-char
 :i  "C-S-d"   #'backward-kill-word)



(setq iedit-toggle-key-default nil) ;; disable auto bind M-;
(map! :after iedit :map iedit-mode-keymap
      :n "s-;"          #'iedit-toggle-selection
      :n "s-'"          #'iedit-show/hide-context-lines)

(map! :after isearch-mode :map isearch-mode-map
      "M-v"             #'isearch-yank-pop
      "M-<backspace>"   #'isearch-delete-char)

(map! :after (company yasnippet) :map company-active-map
      :i  [return]       #'company-complete-selection
      :i  "RET"          #'company-complete-selection
      :ig "TAB"          yas-maybe-expand
      :ig [tab]          yas-maybe-expand)

(map! :after git-rebase :map git-rebase-mode-map
      :n "s-j" #'git-rebase-move-line-down
      :n "s-k" #'git-rebase-move-line-up)

(map! :after org :map org-mode-map
      (:when IS-MAC
        :n [s-return]   #'org-meta-return
        :n [s-M-return] #'org-insert-subheading
        :n "s-j"        #'org-metadown
        :n "s-k"        #'org-metaup
        :n "s-h"        #'org-metaleft
        :n "s-l"        #'org-metaright
        :n "s-J"        #'org-shiftmetadown
        :n "s-K"        #'org-shiftmetaup
        :n "s-H"        #'org-shiftmetaleft
        :n "s-L"        #'org-shiftmetaright)

      ;; Override `xref-pop-marker-stack'
      [remap evil-jump-backward]    #'zc-org/outline-previous-mark
      [remap xref-go-back]          #'zc-org/outline-previous-mark
      [remap xref-pop-marker-stack] #'zc-org/outline-previous-mark

      ;; Taken from `evil-collection-outline'
      :n "zB" #'outline-hide-body     ; Hide all bodies
      :n "zE" #'outline-hide-entry    ; Hide current body
      :n "ze" #'outline-show-entry    ; Show current body only, not subtree, reverse of outline-hide-entry
      :n "zl" #'outline-hide-leaves   ; Like `outline-hide-body' but for current subtree only
      :n "zK" #'outline-show-branches ; Show all children recursively but no body
      :n "zk" #'outline-show-children ; Direct children only unlike `outline-show-branches'
      :n "zp" #'outline-hide-other ; Hide all nodes and bodies except current body
      :n "zu" #'zc-org/outline-up-heading

      :n "[["  #'outline-previous-visible-heading
      :n "]]"  #'outline-next-visible-heading
      :n "C-k" #'outline-backward-same-level
      :n "C-j" #'outline-forward-same-level
      :n "gk"  #'outline-backward-same-level
      :n "gj"  #'outline-forward-same-level)

;; <leader> w --- window management
(map! :after evil :map evil-window-map
      "=" #'balance-windows
      "-" #'+evil/window-split-and-follow
      "_" #'evil-window-split
      "s" #'+evil/window-vsplit-and-follow
      "S" #'evil-window-vsplit
      "D" #'delete-other-windows

      ;; Override doom default to reduce the keystrike for commonly used commands
      "m" #'doom/window-maximize-buffer
      (:prefix "M"
               "m" #'doom/window-maximize-buffer
               "v" #'doom/window-maximize-vertically
               "s" #'doom/window-maximize-horizontally))


;; Custom help keys -- overrides the default `help-map' defined in
;; `modules/config/default/config.el'.

(define-key! help-map
  ;; new bindings
  "h"    #'helpful-at-point
  "a"    #'consult-apropos
  "m"    #'consult-man

  ;; describe bindings
  "d"    nil
  "dc"   #'describe-char
  "dC"   #'describe-coding-system           "C-c" nil
  "du"   #'doom/help-autodefs               "u"   nil
  "dv"   #'helpful-variable                 "v"   nil
  "dV"   #'doom/help-custom-variable        "V"   nil
  "df"   #'helpful-callable                 "f"   nil
  "dF"   #'describe-face                    "F"   nil
  "dp"   #'doom/help-packages               "p"   nil
  "dP"   #'find-library                     "P"   nil
  "dm"   #'describe-mode                    "m"   nil
  "dM"   #'doom/describe-active-minor-mode  "M"   nil
  "dk"   #'helpful-key                      "k"   nil
  "dK"   #'describe-key-briefly             "C-k" nil
  "dl"   #'describe-language-environment    "C-l" nil
  "di"   #'info-emacs-manual                "C-m" nil

  ;; replaces `doom' commands
  "D"    nil
  "Db"   #'doom/report-bug
  "Dc"   #'doom/goto-private-config-file
  "DC"   #'doom/goto-private-init-file
  "Dd"   #'doom-debug-mode
  "Df"   #'doom/help-faq
  "Dh"   #'doom/help
  "Dl"   #'doom/help-search-load-path
  "DL"   #'doom/help-search-loaded-files
  "Dm"   #'doom/help-modules
  "Dn"   #'doom/help-news
  "DN"   #'doom/help-search-news
  "Dpc"  #'doom/help-package-config
  "Dpd"  #'doom/goto-private-packages-file
  "Dph"  #'doom/help-package-homepage
  "Dpp"  #'doom/help-packages
  "Ds"   #'doom/help-search-headings
  "DS"   #'doom/help-search
  "Dt"   #'doom/toggle-profiler
  "Du"   #'doom/help-autodefs
  "Dv"   #'doom/version
  "Dx"   #'doom/sandbox

  ;; Translate
  "tt"   #'gts-do-translate)



(map! :leader
      ;; <leader>
      :desc "M-x"                            "SPC" #'execute-extended-command
      :desc "Expand region"                    "v" #'er/expand-region

      "1"       #'winum-select-window-1
      "2"       #'winum-select-window-2
      "3"       #'winum-select-window-3
      "4"       #'winum-select-window-4
      "5"       #'winum-select-window-5
      "6"       #'winum-select-window-6
      "7"       #'winum-select-window-7
      "8"       #'winum-select-window-8
      "9"       #'winum-select-window-9

      ;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
       :desc "Indent buffer"                   "i" #'zc/indent-buffer
       :desc "ibuffer"                         "I" #'ibuffer
       :desc "Narrow"                          "w" (cmd! (zc/buffer-narrow 'narrow))
       :desc "Widen"                           "W" (cmd! (zc/buffer-narrow 'widen))
       :desc "Show message buffer"             "`" (cmd! (switch-to-buffer "*Messages*")))

      ;; <leader> e --- error
      (:prefix-map ("e" . "error")
       :desc "Show errors"                     "l" #'zc-flycheck/toggle-error-list
       :desc "Jump to next error"              "n" #'flycheck-next-error
       :desc "Jump to previous error"          "N" #'flycheck-previous-error
       :desc "Explain error"                   "e" #'flycheck-explain-error-at-point
       :desc "Verify flycheck setup"           "v" #'flycheck-verify-setup

       ;; Compile
       :desc "Compile"                         "c" #'compile
       :desc "Recompile"                       "r" #'recompile
       :desc "Compile project"                 "p" #'projectile-compile-project
       :desc "Rerun last compilation"          "R" #'projectile-repeat-last-command)

      ;; <leader> f --- file
      (:prefix-map ("f" . "file")
       :desc "Yank file path"                  "y" #'zc/copy-buffer-name)

      ;; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Magit status"                    "s" #'magit-status
       :desc "Magit blame"                     "B" #'magit-blame
       :desc "Magit blame addition"            "b" #'magit-blame-addition
       :desc "Magit verbose refresh"           "R" #'magit-refresh-verbose) ; To identify magit-status pref bottlenecks

      ;; <leader> j --- jump
      (:prefix-map ("j" . "jump")
       :desc "Goto char"                       "s" #'evil-avy-goto-char
       :desc "Goto char 2"                     "S" #'evil-avy-goto-char-2
       :desc "Goto line"                       "l" #'evil-avy-goto-line

       :desc "sp up (forward)"                 ">" #'sp-up-sexp
       :desc "sp up (backward)"                "<" #'sp-up-sexp

       :desc "sp rewrap"                       "r" #'sp-rewrap-sexp
       :desc "sp unwrap"                       "u" #'sp-unwrap-sexp)

      ;; <leader> n --- note
      (:prefix-map ("n" . "notes")
       :desc "Agenda quit"                     "d" #'org-agenda-quit

       ;; Navigation
       :desc "Search babel"                    "b" (cmd! (zc-org/outline-file-heading 'babel))
       :desc "Search notes"                    "n" (cmd! (zc-org/outline-file-heading 'note))
       :desc "Search work notes"               "w" (cmd! (zc-org/outline-file-heading 'work))

       :desc "Org capture"                     "c" #'org-capture
       :desc "Toggle last org-clock"           "+" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"        "=" #'org-clock-cancel)

      ;; <leader> p --- project
      (:prefix-map ("p" . "project")
       :desc "Refresh project cache"           "g" #'zc/projectile-refresh-cache
       :desc "Switch project"                  "o" #'consult-projectile-switch-project

       ;; Navigation
       :desc "Find project buffer"             "b" #'consult-projectile-switch-to-buffer
       :desc "Find project file"               "f" #'zc/consult-projectile-find-file
       :desc "Find project file in dir"        "l" #'zc/consult-projectile-find-file-in-dir
       :desc "Toggle test file"                "t" #'projectile-toggle-between-implementation-and-test
       :desc "Toggle test file (ow)"           "T" #'projectile-find-implementation-or-test-other-window

       ;; Workspace
       :desc "Switch project layout"           "p" #'zc-layout/switch-project-layout
       :desc "Create project layout"           "P" (cmd! (zc-layout/switch-project-layout t))
       :desc "Close project layout"            "D" #'eyebrowse-close-window-config
       :desc "Previous project layout"     "<tab>" #'eyebrowse-last-window-config)

      ;; <leader> s --- search
      (:prefix-map ("s" . "search")
       :desc "Edit symbol"                     "e" #'evil-iedit-state/iedit-mode
       :desc "Clear highlight"                 "c" #'zc/evil-search-clear-highlight
       :desc "Search forward"                  "h" (cmd! (evil-search-word-forward  1 'symbol))
       :desc "Search backward"                 "H" (cmd! (evil-search-word-backward 1 'symbol)))

      ;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       ;; Apps
       :desc "Show prodigy services"           "P" #'prodigy
       :desc "Show process list"               "p" #'list-processes
       :desc "Docker compose"                  "d" #'docker-compose
       :desc "Emacs debug"                     "D" #'toggle-debug-on-error
       :desc "Explain pause"                   "e" #'docker-compose
       :desc "Auto recompile"                  "R" #'zc-eval/compile-on-save-mode
       :desc "Show imenu"                      "i" #'imenu-list-smart-toggle
       :desc "Show world clock"                "W" #'world-clock)
      )
