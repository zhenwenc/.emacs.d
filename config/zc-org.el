(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'dash-functional)
(require 'general)
(require 'zc-org-funcs)
(require 'zc-hydra-funcs)

(autoload 'counsel-outline-candidates "counsel-projectile")

(defvar org-tempo-tags)
(defvar org-tempo-keywords-alist)



(use-package org
  :straight org-plus-contrib
  :commands org-try-structure-completion
  :defer t

  :general
  (:keymaps 'org-src-mode-map
   "C-c C-c" #'org-edit-src-exit)

  (:states 'normal :keymaps 'org-mode-map
   "RET" #'zc-org/evil-normal-ret)

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
    ("et" counsel-org-tag                         "tag"))

   "Insert"
   (("ia" org-babel-insert-header-arg             "args")
    ("iT" org-table-create-or-convert-from-region "table")
    ("in" org-add-note                            "note"))

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
    ("tI" org-indent-mode                         "indent" :toggle t)
    ("tb" org-show-block-all                      "blocks: show")
    ("tB" org-hide-block-all                      "blocks: hide")))

  :hook
  ((org-mode . visual-line-mode)
   (org-babel-after-execute . zc-org/babel-after-execute))

  :init
  ;; Org file directories must be defined at `:init' block
  ;; so that they are visible to the navigation functions,
  ;; such as `zc-org/goto-agenda-file-heading'.
  (setq org-directory            zc-org/directory
        org-attach-directory     (f-join org-directory "data")
        org-default-notes-file   (f-join org-directory "notes.org")
        org-default-todos-file   (f-join org-directory "todos.org")
        org-default-babel-file   (f-join org-directory "babel.org")
        org-work-todos-file      (f-join org-directory "work/todos.org")
        org-work-notes-directory (f-join org-directory "work"))

  :config
  (setq org-M-RET-may-split-line nil
        org-blank-before-new-entry '((heading         . auto)
                                     (plain-list-item . nil))
        org-indirect-buffer-display 'current-window
        org-insert-heading-respect-content t

        ;; Hide empty line on shift-tab
        ;;
        ;; We usually add extra empty line at the end of a
        ;; subtree for properly display babel result.
        org-cycle-separator-lines 3

        ;; FIXME Is this correct?
        org-catch-invisible-edits 'smart

        ;; Disabled globally as it causes weird issue.
        org-startup-indented nil

        ;; Reduce search results.
        org-imenu-depth 3
        org-refile-targets '((nil              :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))

  (setq org-eldoc-breadcrumb-separator " → "
        org-ellipsis (if (char-displayable-p ?) " ▼" nil)
        org-image-actual-width nil
        org-pretty-entities nil
        org-tags-column 0
        org-use-sub-superscripts '{}

        ;; Hide markers for structural markup elements:
        ;;
        ;;   *bold* → bold
        ;;
        ;; It doesn't work well with evil-mode :(
        org-hide-emphasis-markers nil)

  (setq org-enforce-todo-dependencies t
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "MAYBE(m)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(N)" "❓(M)" "|" "✔(D)" "✘(C)")))

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
                 '(file org-default-notes-file))

                (entry
                 "t" "Todo" "* TODO %?\n%i"
                 '(file+headline org-default-todos-file "Inbox")
                 :kill-buffer t)

                (entry
                 "n" "Note" "* %?\n%(zc-org/capture-code-snippet \"%F\")"
                 '(file org-default-notes-file))

                (entry
                 "T" "Work Todo" "* TODO %?\n%i"
                 '(file+headline org-work-todos-file "Inbox")
                 :kill-buffer t)

                (entry
                 "r" "Read later" "* MAYBE %i%? :Read:"
                 '(file+olp org-default-notes-file)))))

  ;; Babel
  (setq org-src-window-setup 'current-window
        ;; Use major-mode indentation
        org-src-preserve-indentation t
        ;; This cause TAB on src block behaves quite weird.
        org-src-tab-acts-natively nil
        ;; Ask for confirmation before executing src block.
        org-confirm-babel-evaluate #'zc-org/babel-confirm-evaluate)

  ;; Activate babel source code blocks
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (js         . t)
                                 (sql        . t)
                                 (shell      . t)
                                 (python     . t)
                                 (ipython    . nil)
                                 (plantuml   . t)
                                 (restclient . t)))

  ;; A bit hacky, but works fine for myself :p
  ;; Alternatively we can use `ts-node' instead.
  (defalias 'org-babel-execute:typescript 'org-babel-execute:js)

  ;; TODO Let's improve TypeScript experience
  ;; http://rwx.io/posts/org-with-babel-node-updated/
  ;;
  ;;   (defun org-babel-execute:typescript (body params)
  ;;     "Execute a block of Typescript code with org-babel.
  ;; This function is called by `org-babel-execute-src-block'."
  ;;     (org-babel-execute:js body params))

  ;; (setq org-babel-js-cmd "ts-node")
  ;; (setq org-babel-js-function-wrapper "require('process').stdout.write(require('util').inspect(function(){%s}()));")
  ;; (setenv "NODE_PATH"
  ;;         (concat
  ;;          (getenv "HOME") "/somewhere/node_modules" ":"
  ;;          (getenv "NODE_PATH")))

  ;; Enable LSP Mode for babel source block
  ;; - centaur-emacs
  (defun org-babel-edit-prep:python (info)
    (let ((file-name (->> info caddr (alist-get :file))))
      (unless file-name
        (setq file-name (f-join org-babel-temporary-directory "edit.py")))
      (setq buffer-file-name file-name)
      (lsp-deferred))))



(use-package org-agenda
  :straight org-plus-contrib
  :after org
  :commands (org-agenda)

  :general
  (:states 'motion :keymaps 'org-agenda-mode-map
   "RET" #'org-agenda-switch-to
   "sc"  #'zc-org/agenda-filter-by-category
   "sC"  #'org-agenda-filter-by-category)

  :hydra
  ("Basic"
   (("."  org-agenda-goto-today "goto today")
    ("+"  org-agenda-manipulate-query-add "query add")
    ("-"  org-agenda-manipulate-query-subtract "query remove")
    ("%"  org-agenda-bulk-mark-regexp "bulk mark regexp")
    ("*"  org-agenda-bulk-mark-all "bulk mark all")
    ("~"  org-agenda-bulk-toggle-all "bulk toggle all")
    ("x"  org-agenda-bulk-action "bulk action")
    ("r"  org-agenda-redo "rebuild agenda view")
    ("C"  org-agenda-capture "capture"))

   "Search"
   (("sh" org-agenda-filter-by-top-headline "filter by headline")
    ("sc" zc-org/agenda-filter-by-category "filter by category")
    ("sC" org-agenda-filter-by-category "filter by category at point")
    ("se" org-agenda-filter-by-effort "filter by effort")
    ("sr" org-agenda-filter-by-regexp "filter by regexp")
    ("st" org-agenda-filter-by-tag "filter bt tag")
    ("ss" org-agenda-limit-interactively "limit interactively")
    ("S"  org-agenda-filter-remove-all "clear filter"))

   "Clock & Set"
   (("ct" org-agenda-set-tags "set tags")
    ("cT" org-timer-set-timer "set timer")
    ("ce" org-agenda-set-effort "set effort")
    ("cc" org-agenda-clock-cancel "clock cancel")
    ("cg" org-agenda-clock-goto "clock goto")
    ("cr" org-agenda-clockreport-mode "clock report"))

   "Toggle"
   (("tf" org-agenda-follow-mode "follow")
    ("te" org-agenda-entry-text-show "entry text"))

   "Archive"
   (("da" org-agenda-archive-default-with-confirmation "archive y/n")
    ("dA" org-agenda-archive "archive")
    ("dd" org-agenda-kill "kill entry"))

   "Navigation"
   (("gr" org-agenda-redo "redo")
    ("gR" org-agenda-redo-all "redo all")
    ("gc" org-agenda-goto-calendar "goto calendar")
    ("gC" org-agenda-convert-date "convert date")
    ("gd" org-agenda-goto-date "goto date")
    ("gt" org-agenda-show-tags "show tags")))

  :hook
  ((org-agenda-after-show . org-narrow-to-subtree))

  :init
  ;; Org file directories must be defined at `:init' block
  ;; so that they are visible to the navigation functions,
  ;; such as `zc-org/goto-agenda-file-heading'.
  (setq org-agenda-diary-file  (f-join zc-org/directory "diary.org")
        org-agenda-files       (zc-org/file-with-exts '("org")))

  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'reorganize-frame)

  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers))


;; https://orgmode.org/manual/Easy-templates.html
;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates

(use-package org-tempo
  :after org
  :config
  ;; Complete src block templates with uppercased keywords
  (defun zc-org/post-tempo-add-templates ()
    (mapc #'(lambda (entry)
              (let* ((key (cdr entry))
                     (value (symbol-value key)))
                (set key (-map-when 'stringp 'upcase value))))
          org-tempo-tags)
    (message "HACK: Org complete templates with uppercase keycords."))
  (advice-add 'org-tempo-add-templates :after #'zc-org/post-tempo-add-templates)

  ;; Expand "#+NAME:" with "<n"
  (add-to-list 'org-tempo-keywords-alist '("n" . "name")))



(use-package org-eldoc
  :after org
  :defer t

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
              hdr-args " "))))))))


;; Org Pretty

(use-package org-superstar
  :straight (:host github :repo "integral-dw/org-superstar-mode")
  :after org
  :hook (org-mode . org-superstar-mode))


;; Org Babel

(use-package ob-restclient
  :straight t
  :after org)

(use-package ob-ipython
  :disabled t ;; not used
  :straight t
  :after org
  :if (executable-find "jupyter"))

(use-package ob-async
  :straight t
  :after org
  :hook (org-mode . (lambda () (require 'ob-async)))
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            `(lambda ()
               (setq org-plantuml-jar-path ,(concat paths-vendor-dir "plantuml.jar"))))
  ;; ipython has its own async keyword
  (add-to-list 'ob-async-no-async-languages-alist "ipython"))



(provide 'zc-org)
