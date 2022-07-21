(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'general)
(require 'zc-hydra-funcs)



(use-package org
  :straight t
  :after (:and ob-mermaid)

  :general
  (:keymaps 'org-mode-map
   [remap xref-find-definitions] #'org-open-at-point
   [remap xref-pop-marker-stack] #'zc-org/goto-previous-mark)

  (:keymaps 'org-src-mode-map
   "C-c C-c" #'org-edit-src-exit)

  (:states 'normal :keymaps 'org-mode-map
   "RET" #'zc-org/evil-normal-ret
   "C-e" #'evil-org-end-of-line)

  (:states 'normal :keymaps 'outline-mode-map
   [remap outline-up-heading] #'zc-org/outline-up-heading)

  (:states 'insert :keymaps 'org-mode-map
   ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun overriding
   ;; org's [tab] keybind in GUI Emacs. This is needed to undo that.
   [tab] #'org-cycle)

  :hydra
  ("Basic"
   (("?" org-info                                 "org info")
    ("E" org-export-dispatch                      "export")
    ("P" zc-org/export-pdf-and-open               "export pdf")
    ("S" org-sort                                 "sort")
    ("R" org-refile                               "refile"))

   "Edit"
   (("ee" org-edit-special                        "edit")
    ("ed" org-cut-special                         "kill")
    ("ey" org-copy-special                        "copy")
    ("eY" org-paste-special                       "paste")
    ("ep" org-property-action                     "props")
    ("et" counsel-org-tag                         "tag")
    ("en" org-add-note                            "note"))

   "Babel"
   (("bi" org-babel-view-src-block-info           "info")
    ("bv" org-babel-expand-src-block              "expand")
    ("bo" org-babel-open-src-block-result         "open res")
    ("bc" org-babel-remove-result-one-or-many     "clear res")
    ("bC" zc-org/babel-remove-result-all          "clear res*"))

   "Toggle"
   (("th" org-toggle-heading                      "heading")
    ("ti" org-toggle-item                         "item")
    ("tl" org-toggle-link-display                 "link")
    ("tp" org-appear-mode                         "appear" :toggle t)
    ("tI" org-indent-mode                         "indent" :toggle t)
    ("tb" org-show-block-all                      "blocks: show")
    ("tB" org-hide-block-all                      "blocks: hide")
    ("tt" org-sidebar-tree-toggle                 "sidebar tree")))

  :hook
  ((org-mode . visual-line-mode)
   (org-mode . (lambda ()
                 (setq prettify-symbols-alist '(("#+BEGIN_SRC"   . "✎")
                                                ("#+END_SRC"     . "□")
                                                ("#+BEGIN_QUOTE" . "»")
                                                ("#+END_QUOTE"   . "«")))
                 (prettify-symbols-mode)))
   (org-babel-after-execute . zc-org/babel-after-execute))

  :init
  (require 'zc-org-funcs)

  ;; Org file directories must be defined at `:init' block
  ;; so that they are visible to the navigation functions,
  ;; such as `zc-org/goto-agenda-file-heading'.
  (setq org-directory          (f-expand zc-org/directory)
        org-attach-id-dir      (f-join org-directory "data")

        org-default-notes-file (f-join zc-org/main-notes-dir "notes.org")
        org-default-todos-file (f-join zc-org/main-notes-dir "todos.org")
        org-default-babel-file (f-join zc-org/main-notes-dir "babel.org")
        org-agenda-diary-file  (f-join zc-org/main-notes-dir "diary.org")

        org-agenda-files       (append (zc-org/file-with-exts :dir zc-org/directory)
                                       (zc-org/file-with-exts :dir zc-org/main-notes-dir)
                                       (zc-org/file-with-exts :dir zc-org/work-notes-dir)))

  :config
  (setq org-M-RET-may-split-line nil
        org-insert-heading-respect-content t
        org-indirect-buffer-display 'current-window
        org-blank-before-new-entry '((heading         . auto)
                                     (plain-list-item . nil))
        ;; Hide empty line on shift-tab
        ;;
        ;; We usually add extra empty line at the end of a
        ;; subtree for properly display babel result.
        org-cycle-separator-lines 3
        ;; FIXME Is this correct?
        org-catch-invisible-edits 'smart
        ;; Disabled globally as it causes weird issue.
        org-startup-indented nil
        ;; Fold all contents on opening a org file.
        org-startup-folded t
        ;; Enable alphabetical bullet lists.
        org-list-allow-alphabetical t
        ;; Reduce search results.
        org-imenu-depth 3
        org-refile-targets `((nil              :maxlevel . 1)
                             (org-agenda-files :regexp   . ,(rx "Tasks"))))

  ;; Appearance
  (setq org-eldoc-breadcrumb-separator " → "
        org-ellipsis (if (char-displayable-p ?) " ▼" nil)
        org-tags-column 0
        ;; Do not display image actual width
        org-image-actual-width 500
        ;; Inhibit displaying TeX-like syntax for "_" and "^"
        org-use-sub-superscripts '{})

  ;; Babel
  (setq org-src-window-setup 'current-window
        ;; Disable reindent on every time editing code block
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        ;; This cause TAB on src block behaves quite weird.
        org-src-tab-acts-natively nil
        ;; Ask for confirmation before executing src block.
        org-confirm-babel-evaluate #'zc-org/babel-confirm-evaluate)

  ;; Performance boost: Disable the unused modules
  ;; (setq org-modules (cl-set-difference org-modules '(ol-gnus ol-eww)))

  ;; Default todo keywords and workflow
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "MAYBE(m)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(N)" "❓(M)" "|" "✔(D)" "✘(C)"))
        org-enforce-todo-dependencies t)

  ;; Basic pre-defined tags for `org-set-tags-command'.
  (setq org-tag-alist '(("@home"  . ?h)
                        ("@work"  . ?w)
                        ("note"   . ?n)
                        ("idea"   . ?i)))

  ;; TODO Refactor template definitions with Declarative Org Capture Templates
  ;; https://github.com/progfolio/doct
  (setq org-capture-templates
        (cl-labels ((entry
                     (key label template target
                          &rest properties
                          &key
                          (type 'entry)
                          (prepend t)
                          (clock-keep t)
                          (empty-lines 1)
                          &allow-other-keys)
                     (append
                      (list key label type target template
                            :clock-keep clock-keep
                            :empty-lines empty-lines
                            :prepend prepend)
                      properties)))

          (list (entry
                 "i" "Idea" "* MAYBE %?\n%i :idea:"
                 '(file+headline org-default-todos-file "Inbox"))

                (entry
                 "t" "Todo" "* TODO %?\n%i"
                 '(file+headline org-default-todos-file "Inbox")
                 :kill-buffer t)

                (entry
                 "n" "Note" "* %?\n%(zc-org/capture-code-snippet \"%F\")"
                 '(file+headline org-default-todos-file "Inbox"))

                (entry
                 "r" "Read later" "* MAYBE %i%? :Read:"
                 '(file+olp org-default-notes-file)))))

  ;; Integration with `restclient'.
  (use-package ob-restclient :straight t)

  ;; Disabled due to variables will be evaluated twice.
  (use-package ob-async
    :disabled t
    :straight t
    :hook (org-mode . (lambda () (require 'ob-async)))
    :config
    (add-hook 'ob-async-pre-execute-src-block-hook
              `(lambda ()
                 (setq org-plantuml-jar-path ,(concat paths-vendor-dir "plantuml.jar"))))
    ;; ipython has its own async keyword
    (add-to-list 'ob-async-no-async-languages-alist "ipython"))

  ;; Activate babel source code blocks
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (js         . t)
                                 (sql        . t)
                                 (shell      . t)
                                 (python     . t)
                                 (ipython    . nil)
                                 (mermaid    . t)
                                 (plantuml   . t)
                                 (restclient . t)))

  ;; Alias source code block languages
  (add-to-list 'org-src-lang-modes '("rust"       . rustic))
  (add-to-list 'org-src-lang-modes '("ts"         . typescript))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript))
  (add-to-list 'org-src-lang-modes '("json"       . zc-web-json))

  ;; Inhibit displaying message in echo-area while resolving src-block info.
  ;; This is annoying when executing a code block which extracts remote params
  ;; from other code blocks, where the intermediate results will be printed to
  ;; the echo-area that causes flickering effect.
  (defun zc-org/inhibit-message (orig-fn &rest args)
    (let ((inhibit-message t)) (message (make-string 60 ?-)) (apply orig-fn args)))
  (advice-add 'org-babel-get-src-block-info      :around #'zc-org/inhibit-message)
  (advice-add 'org-babel-expand-noweb-references :around #'zc-org/inhibit-message)
  (advice-add 'org-babel-execute:restclient      :around #'zc-org/inhibit-message))



(use-package org-agenda
  :straight org
  :commands (org-agenda)

  :general
  (:states 'motion :keymaps 'org-agenda-mode-map
   "RET" #'org-agenda-switch-to
   ;; The default keybinding `:' had been occupied by `evil'.
   "T"   #'org-agenda-set-tags)

  :hydra
  ("Basic"
   (("."  org-agenda-goto-today                "goto today")
    ("+"  org-agenda-manipulate-query-add      "query add")
    ("-"  org-agenda-manipulate-query-subtract "query remove")
    ("*"  org-agenda-bulk-mark-all             "bulk mark all")
    ("~"  org-agenda-bulk-toggle-all           "bulk toggle all")
    ("R"  org-agenda-bulk-mark-regexp          "bulk mark regexp")
    ("x"  org-agenda-bulk-action               "bulk action")
    ("r"  org-agenda-redo                      "rebuild agenda view")
    ("C"  org-agenda-capture                   "capture"))

   "Search"
   (("sh" org-agenda-filter-by-top-headline    "filter by headline")
    ("sc" zc-org/agenda-filter-by-category     "filter by category")
    ("sC" org-agenda-filter-by-category        "filter by category at point")
    ("se" org-agenda-filter-by-effort          "filter by effort")
    ("sr" org-agenda-filter-by-regexp          "filter by regexp")
    ("st" org-agenda-filter-by-tag             "filter bt tag")
    ("ss" org-agenda-limit-interactively       "limit interactively")
    ("sK" org-agenda-filter-remove-all         "clear all filters"))

   "Clock & Set"
   (("ct" org-agenda-set-tags                  "set tags")
    ("cT" org-timer-set-timer                  "set timer")
    ("ce" org-agenda-set-effort                "set effort")
    ("cc" org-agenda-clock-cancel              "clock cancel")
    ("cg" org-agenda-clock-goto                "clock goto")
    ("cr" org-agenda-clockreport-mode          "clock report"))

   "Toggle"
   (("tf" org-agenda-follow-mode               "follow")
    ("te" org-agenda-entry-text-show           "entry text"))

   "Archive"
   (("da" org-agenda-archive-default-with-confirmation "archive y/n")
    ("dA" org-agenda-archive                   "archive")
    ("dd" org-agenda-kill                      "kill entry"))

   "Navigation"
   (("gr" org-agenda-redo                      "redo")
    ("gR" org-agenda-redo-all                  "redo all")
    ("gc" org-agenda-goto-calendar             "goto calendar")
    ("gC" org-agenda-convert-date              "convert date")
    ("gd" org-agenda-goto-date                 "goto date")
    ("gt" org-agenda-show-tags                 "show tags")))

  :hook
  ((org-agenda-after-show . org-narrow-to-subtree))

  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'reorganize-frame)

  (setq org-agenda-start-with-log-mode t
        org-log-into-drawer t
        org-log-done 'time)

  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers))

;; https://orgmode.org/manual/Easy-templates.html
;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
(use-package org-tempo
  :config
  ;; Complete src block templates with uppercased keywords
  (defun zc-org/post-tempo-add-templates ()
    (mapc #'(lambda (entry)
              (let* ((key (cdr entry))
                     (value (symbol-value key)))
                (set key (-map-when 'stringp 'upcase value))))
          org-tempo-tags)
    (message "HACK: Org complete templates with uppercase keycords."))
  (advice-add 'org-tempo-add-templates :after 'zc-org/post-tempo-add-templates)

  ;; Expand "#+NAME:" with "<n"
  (add-to-list 'org-tempo-keywords-alist '("n" . "name")))

(use-package org-eldoc
  ;; HACK: Error when header argument value is numeric type, such as port number.
  :config/el-patch
  (defun org-eldoc-get-src-header ()
    "Returns lang and list of header properties if on src
definition line and nil otherwise."
    (let ((case-fold-search t) info lang hdr-args)
      (save-excursion
        (beginning-of-line)
        (save-match-data
          (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
            (setq info (org-babel-get-src-block-info 'light)
                  lang (propertize (or (nth 0 info) "no lang")
                                   'face 'font-lock-string-face)
                  hdr-args (nth 2 info))
            (concat
             lang
             ": "
             (mapconcat
              (lambda (elem)
                (el-patch-let (($old (cdr elem))
                               ($new (format "%s" (cdr elem))))
                  (when (and (cdr elem) (not (string= "" (el-patch-swap $old $new))))
                    (concat
                     (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                     " "
                     (propertize (el-patch-swap $old $new) 'face 'org-verbatim)
                     " "))))
              hdr-args " ")))))))
  :config
  (defun zc-org/post-org-eldoc-get-breadcrumb (orig-fn)
    "Unify text line-height for outline headers with `org-level-*' face,
so that the breadcrumb will fit in the default echo area."
    (when-let ((text (funcall orig-fn)) (face '(:height 1)) (pos 0) (end 0))
      (setq pos (next-single-property-change 0 'face text))
      (while pos
        (setq end (next-single-property-change pos 'face text))
        (add-face-text-property pos (or end (length text)) face nil text)
        (setq pos (next-single-property-change pos 'face text)))
      text))
  (advice-add 'org-eldoc-get-breadcrumb :around #'zc-org/post-org-eldoc-get-breadcrumb))



(use-package org-roam
  :disabled t
  :straight t
  :if (executable-find "cc")
  :hook (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :custom
  (org-roam-directory   zc-org/directory)
  (org-roam-db-location (f-join paths-cache-dir "org-roam.db")))


;; Appearance

(use-package org-superstar
  :straight (:host github :repo "integral-dw/org-superstar-mode")
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Remove the leading dots on headline
  (org-superstar-leading-bullet "  ")
  ;; Change org unordered list styles.
  (org-superstar-prettify-item-bullets t)
  (org-superstar-item-bullet-alist '((?* . ?•)
                                     (?+ . ?•)
                                     (?- . ?•))))

(use-package org-appear
  :straight (:host github :repo "awth13/org-appear")
  :init
  ;; Instant toggle raw format on insert mode, 1 second delay on normal mode.
  (add-hook 'evil-insert-state-entry-hook (lambda() (setq org-appear-delay 0)))
  (add-hook 'evil-normal-state-entry-hook (lambda() (setq org-appear-delay 1)))
  :custom
  (org-appear-delay          0)
  (org-appear-autolinks      t)
  (org-appear-autoentities   t)
  (org-appear-autokeywords   t)
  (org-appear-autosubmarkers t)
  :config
  ;; Hide markers for structural markup elements:
  ;;
  ;;   *bold* → bold
  ;;
  (setq org-hide-emphasis-markers t)
  ;; Prettify things like \pi, sub/super script.
  (setq org-pretty-entities t))

(use-package org-sidebar
  :straight (:host github :repo "alphapapa/org-sidebar"))


;; Misc.

;; Export to Github Flavored Markdown
(use-package ox-gfm :straight t)



(provide 'zc-org)
