(eval-when-compile
  (require 'el-patch)
  (require 'use-package))

(require 's)
(require 'subr-x)
(require 'general)
(require 'zc-funcs)
(require 'zc-hydra-funcs)
(require 'zc-layout-funcs)

(autoload 'counsel-imenu "counsel")
(autoload 'counsel-recentf "counsel")
(autoload 'counsel-find-file "counsel")
(autoload 'counsel-faces "counsel")
(autoload 'counsel-grep-or-swiper "counsel")
(autoload 'counsel-projectile "counsel-projectile")
(autoload 'counsel-projectile-rg "counsel-projectile")
(autoload 'counsel-projectile-find-dir "counsel-projectile")
(autoload 'projectile-ibuffer "projectile")
(autoload 'ivy-switch-buffer "ivy-switch-buffer")

(autoload 'magit-status "magit")
(autoload 'magit-blame "magit")
(autoload 'magit-log-current "magit")
(autoload 'magit-log-buffer-file "magit")
(autoload 'git-timemachine "git-timemachine")

(autoload 'deft "deft")
(autoload 'ace-swap-window "ace-window")
(autoload 'ace-select-window "ace-window")
(autoload 'ace-delete-window "ace-window")
(autoload 'ace-delete-other-windows "ace-window")

(autoload 'treemacs "treemacs")
(autoload 'treemacs-select-window "treemacs")

(autoload 'helpful-key "helpful")
(autoload 'helpful-macro "helpful")
(autoload 'helpful-command "helpful")
(autoload 'helpful-variable "helpful")
(autoload 'helpful-callable "helpful")
(autoload 'helpful-at-point "helpful")

(autoload 'youdao-dictionary-search "youdao-dictionary")
(autoload 'youdao-dictionary-search-from-input "youdao-dictionary")

(autoload 'org-agenda "org-agenda")
(autoload 'org-capture "org-capture")



(use-package pretty-hydra
  :straight (:host github :repo "jerrypnz/major-mode-hydra.el")
  :after (hydra)

  :config
  (zc-hydra/define zc-main-hydra--window
    (:color teal :title "Window Hydra" :icon "windows" :prefix "w")
    ("Basic"
     (("w SPC" zc/toggle-current-window-dedication "dedicate")
      ("wp" ivy-push-view "push view")
      ("wP" ivy-switch-view "switch view")
      ("wo" ace-select-window "select")
      ("w0" (text-scale-set 0) "scale reset")
      ("w+" text-scale-increase "scale ↑" :color pink)
      ("w_" text-scale-decrease "scale ↓" :color pink))

     "Split"
     (("w-" zc/split-window-below-and-focus "vertical")
      ("ws" zc/split-window-right-and-focus "horizontal")
      ("wR" ace-swap-window "swap")
      ("wr" evil-window-rotate-downwards "rotate"))

     "Resize"
     (("w=" balance-windows "balance")
      ("wm" zc/toggle-maximize-window "maximize")
      ("w," shrink-window "resize ←" :color pink)
      ("w." enlarge-window "resize ↑" :color pink)
      ("w<" shrink-window-horizontally "resize ←" :color pink)
      ("w>" enlarge-window-horizontally "resize →" :color pink))

     "Killer"
     (("wd" delete-window "kill")
      ("wD" delete-other-windows "close others")
      ("wk" ace-delete-window "ace kill")
      ("wK" ace-delete-other-windows "ace close others")
      ("wQ" zc/kill-emacs-or-frame "kill frame"))))

  (zc-hydra/define zc-main-hydra--buffer
    (:color teal :title "Buffer & Bookmark Hydra" :icon "drupal" :prefix "b")
    ("Basic"
     (("b SPC" (zc/buffer-narrow 'toggle)      "Toggle narrow")
      ("bw" (zc/buffer-narrow 'narrow)         "Narrow")
      ("bW" (zc/buffer-narrow 'widen)          "Widen"))

     "Navigation"
     (("bb" ivy-switch-buffer                  "Switch buffer")
      ("bn" projectile-next-project-buffer     "Switch to next")
      ("bN" projectile-previous-project-buffer "Switch to previous")
      ("bS" (switch-to-buffer "*scratch*")     "Switch to scratch")
      ("bM" (switch-to-buffer "*Messages*")    "Switch to messages")
      ("bz" bury-buffer                        "Bury buffer"))

     "Buffer"
     (("bB" ibuffer                            "ibuffer")
      ("bi" zc/indent-buffer                   "Indent buffer")
      ("bc" zc/buffer-clone-indirect           "Clone buffer")
      ("bd" zc-layout/kill-buffer              "Kill buffer")
      ("by" zc/copy-buffer-to-clipboard        "Yank buffer"))

     "Bookmark"
     (("bm" bookmark-set                       "Set bookmark")
      ("bj" bookmark-jump                      "Jump to bookmark")
      ("bl" bookmark-bmenu-list                "List bookmark"))))

  (zc-hydra/define zc-main-hydra--file
    (:color teal :title "File Hydra" :icon "file" :prefix "f")
    ("Basic"
     (("fs" save-buffer "save")
      ("fv" reload-file "reload")
      ("fD" zc/delete-buffer-and-file "delete")
      ("fR" zc/rename-buffer-and-file "rename")
      ("fb" counsel-bookmark "bookmark"))

     "Search"
     (("fr" counsel-recentf "find recent")
      ("ff" counsel-find-file "find file")
      ("fo" find-file-other-window "find file (other window)")
      ("fO" zc-osx/finder "open directory in Finder"))

     "Copy"
     (("fc" zc/copy-file "copy file")
      ("fy" zc/copy-buffer-name "copy file name")
      ("fY" zc/copy-buffer-path "copy file path"))

     "Treemacs"
     (("fT" treemacs-select-window "treemacs show")
      ("ft" treemacs "treemacs toggle")
      ("fp" treemacs-add-and-display-current-project "treemacs project")
      ("fP" treemacs-display-current-project-exclusively "treemacs project (E)"))))

  (zc-hydra/define zc-main-hydra--symbol
    (:color teal :title "Symbol Hydra" :icon "strikethrough" :prefix "s")
    ("Basic"
     (("sj" (zc-ivy/imenu) "imenu")
      ("sJ" (zc-ivy/imenu t) "imenu widen")
      ("sl" imenu-list "imenu list")
      ("su" counsel-unicode-char "unicode"))

     "Symbol"
     (("se" evil-iedit-state/iedit-mode "edit")
      ("sh" zc-evil-ahs/highlight-symbol "highlight")
      ("sH" zc-evil-ahs/goto-last-searched-symbol "goto last searched")
      ("sc" zc/evil-search-clear-highlight "clear highlights"))

     "Search"
     (("ss" counsel-grep-or-swiper "swiper")
      ("sp" counsel-projectile-rg "search project")
      ("sP" zc-projectile/search-symbol-at-point "search project (sym)"))))

  (zc-hydra/define zc-main-hydra--project
    (:color teal :title "Project Hydra" :icon "product-hunt" :prefix "p")
    ("Basic"
     (("p!" projectile-run-shell-command-in-root "shell command")
      ("pk" projectile-kill-buffers "kill buffers")
      ("pi" ibuffer "ibuffer")
      ("pI" projectile-invalidate-cache "invalidate cache")
      ("po" counsel-projectile-switch-project "switch project")
      ("pg" zc-projectile/refresh-projects "refresh projects"))

     "Search"
     (("pf" counsel-projectile-find-file "find file")
      ("pl" projectile-find-file-in-directory "find in directory")
      ("pb" counsel-projectile-switch-to-buffer "find buffer")
      ("pd" counsel-projectile-find-dir "find directory")
      ("pt" projectile-toggle-between-implementation-and-test "toggle test file")
      ("pT" projectile-find-implementation-or-test-other-window "open test file (ow)"))

     "Layer"
     (("pp" zc-layout/switch-project-layout "switch layout")
      ("pP" (zc-layout/switch-project-layout t) "create layout")
      ("pD" eyebrowse-close-window-config "close layout")
      ("p$" counsel-tramp "tramp")
      ("p <tab>" eyebrowse-last-window-config "last project"))))

  (zc-hydra/define zc-main-hydra--git
    (:color teal :title "Git Hydra" :icon "github" :prefix "g")
    ("Basic"
     (("gs" magit-status "status")
      ("gl" magit-log-buffer-file "file log")
      ("gL" magit-log-current "project log")
      ("gt" git-timemachine "time machine")
      ("gb" magit-blame "blame")
      ("gB" browse-at-remote "browse remote")
      ("gy" browse-at-remote-kill "copy remote link"))))

  (zc-hydra/define zc-main-hydra--error
    (:color teal :title "Error & Eval Hydra" :icon "bug" :prefix "e")
    ("Flycheck"
     (("el" zc-flycheck/toggle-error-list   "show errors")
      ("eL" zc-flycheck-hydra/body          "show errors (hydra)")
      ("en" flycheck-next-error             "next error")
      ("eN" flycheck-previous-error         "prev error")
      ("ee" flycheck-explain-error-at-point "explain error")
      ("ev" flycheck-verify-setup           "verify setup"))

     "Compilation"
     (("ec" compile                         "compile")
      ("er" recompile                       "recompile")
      ("ep" projectile-compile-project      "project compile")
      ("eP" projectile-test-project         "project test")
      ("eR" projectile-repeat-last-command  "project recompile"))))

  (zc-hydra/define zc-main-hydra--org
    (:color teal :title "Org Hydra" :icon "empire" :prefix "o")
    ("Agenda"
     (("oa" org-agenda      "agenda")
      ("oc" org-capture     "capture")
      ("ot" org-todo-list   "todo list")
      ("od" org-agenda-quit "quit"))

     "View"
     (("os" org-ql-search   "search")
      ("ov" org-ql-view     "view"))

     "Navigation"
     (("oo" (zc-org/goto-file-heading 'all)   "all")
      ("ow" (zc-org/goto-file-heading 'work)  "work")
      ("on" (zc-org/goto-file-heading 'note)  "notes")
      ("ob" (zc-org/goto-file-heading 'babel) "babel"))))

  (zc-hydra/define zc-main-hydra--jump
    (:color teal :title "Jump Hydra" :icon "tencent-weibo" :prefix "j")
    ("Basic"
     (("jj" evil-avy-goto-char "goto char")
      ("jJ" evil-avy-goto-char-2 "goto char 2")
      ("jl" evil-avy-goto-line "goto line")
      ("js" xref-find-apropos "find symbol"))

     "Navigation"
     (("jf" sp-forward-sexp "sp forward")
      ("jb" sp-backward-sexp "sp backward")
      ("je" sp-end-of-sexp "sp end")
      ("jE" sp-beginning-of-sexp "sp beginning")
      ("jn" sp-next-sexp "sp next")
      ("jN" sp-previous-sexp "sp previous")
      ("j>" sp-up-sexp "sp up (end)" :color red)
      ("j<" sp-backward-up-sexp "sp up (start)" :color red))

     "Wrapping"
     (("jr" sp-rewrap-sexp "sp rewrap")
      ("ju" sp-unwrap-sexp "sp unwrap"))))

  (zc-hydra/define zc-main-hydra--toggle
    (:color teal :title "Toggle Hydra" :icon "building" :prefix "t")
    ("Basic"
     (("tf" toggle-frame-fullscreen "fullscreen")
      ("tF" toggle-frame-maximized "maximize frame")
      ("tD" toggle-debug-on-error "emacs debug" :toggle (default-value 'debug-on-error))
      ("tn" linum-mode "line numbers" :toggle t)
      ("tk" which-key-mode "which key" :toggle t))

     "Edit"
     (("tw" whitespace-mode "whitespace" :toggle t)
      ("tl" visual-line-mode "visual line" :toggle t)
      ("tL" auto-fill-mode "fill line" :toggle (symbol-value 'auto-fill-function))
      ("tc" display-fill-column-indicator-mode "indicate column" :toggle t)
      ("tC" visual-fill-column-mode "visual column" :toggle t)
      ("tS" flyspell-mode "spell check" :toggle t)
      ("tE" explain-pause-mode "monitor pause" :toggle t))

     "Apps"
     (("tp" prodigy "prodigy services")
      ("tP" list-processes "process list")
      ("td" docker "docker")
      ("te" explain-pause-top "explain pause")
      ("tR" zc-eval/compile-on-save-mode "auto recompile")
      ("ti" imenu-list-smart-toggle "imenu list")
      ("tW" display-time-world "world clock"))))

  (zc-hydra/define zc-main-hydra--help
    (:color teal :title "Help Hydra" :icon "question" :prefix "h")
    ("Docs"
     (("hi" info "info")
      ("hI" counsel-info-lookup-symbol "find info")
      ("hm" man "man")
      ("hh" helpful-at-point "doc at point")
      ("hl" counsel-find-library "find library"))

     "Describe"
     (("hdc" describe-char "char")
      ("hdC" zc-ivy/describe-command "command")
      ("hdv" counsel-describe-variable "variable")
      ("hdk" helpful-key "key")
      ("hdf" counsel-describe-function "function")
      ("hdF" counsel-faces "face")
      ("hdm" describe-mode "mode")
      ("hdM" helpful-macro "macro")
      ("hdp" describe-package "package"))

     "Translate"
     (("htt" go-translate "translate"))))

  (zc-hydra/define zc-main-hydra
    (:hint nil :color teal :title "Main Hydra")
    ("Basic"
     (("SPC" counsel-M-x               "M-x")
      (":"   eval-expression           "Eval expression")
      ("!"   zc-term/open              "Open terminal")
      ("r"   ivy-resume                "Ivy resume")
      ("v"   er/expand-region          "Expand region")
      ("m"   zc-hydra/major-mode-hydra "Major mode hydra")
      ("u"   universal-argument        "Universal argument"))

     "Quick Switch"
     (("1" winum-select-window-1       "window 1")
      ("2" winum-select-window-2       "window 2")
      ("3" winum-select-window-3       "window 3")
      ("4" winum-select-window-4       "window 4")
      ("5" winum-select-window-5       "window 5"))

     ""
     (("b" zc-main-hydra--buffer/body  "+buffer")
      ("e" zc-main-hydra--error/body   "+flycheck")
      ("f" zc-main-hydra--file/body    "+file")
      ("g" zc-main-hydra--git/body     "+git")
      ("h" zc-main-hydra--help/body    "+help")
      ("j" zc-main-hydra--jump/body    "+jump"))

     ""
     (("o" zc-main-hydra--org/body     "+org")
      ("p" zc-main-hydra--project/body "+project")
      ("s" zc-main-hydra--symbol/body  "+symbol")
      ("t" zc-main-hydra--toggle/body  "+toggle")
      ("w" zc-main-hydra--window/body  "+window")))))



(use-package general
  :straight t
  :config
  (general-setq general-override-states
                '(insert emacs hybrid normal visual motion operator replace))
  (general-override-mode +1)
  (general-define-key :keymaps 'override :states '(normal visual motion)
    "SPC" #'zc-main-hydra/body
    ","   #'zc-hydra/major-mode-hydra)
  (general-define-key :keymaps 'override :states '(insert)
    "s-SPC" #'zc-main-hydra/body
    "s-,"   #'zc-hydra/major-mode-hydra))



(use-package hydra
  :straight t
  :if (display-graphic-p)
  :config/el-patch
  ;; HACK: Fix posframe doesn't respect newline character
  ;;       when calculating the child frame height, which
  ;;       causes hydra shows incomplete docstring.
  (defun hydra-posframe-show (str)
    (require 'posframe)
    (when hydra--posframe-timer
      (cancel-timer hydra--posframe-timer))
    (setq hydra--posframe-timer nil)
    (apply #'posframe-show " *hydra-posframe*"
           :string str
           (el-patch-add
             :min-height (1+ (s-count-matches "\n" str)))
           hydra-posframe-show-params))
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `(:internal-border-width 3
          :internal-border-color ,(doom-color 'base2)
          :background-color ,(doom-color 'bg-alt)
          :override-parameters ((alpha 98 98))
          :left-fringe 16
          :right-fringe 16
          :lines-truncate t
          :poshandler posframe-poshandler-frame-bottom-center)))



(provide 'zc-hydra)
