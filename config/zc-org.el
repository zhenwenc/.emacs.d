(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'dash-functional)
(require 'general)
(require 'zc-org-funcs)
(require 'zc-hydra-funcs)

(defvar org-tempo-tags)



(use-package org
  :straight org-plus-contrib
  :defer t

  :general
  (:keymaps 'org-src-mode-map
            "C-c C-c" #'org-edit-src-exit)

  (:states 'normal :keymaps 'org-mode-map
           "RET" #'zc-org/evil-normal-ret)

  (:states 'motion :keymaps 'org-agenda-mode-map
           "RET" #'org-agenda-switch-to
           "sc"  #'zc-org/agenda-filter-by-category
           "sC"  #'org-agenda-filter-by-category)

  :hook
  ((org-agenda-after-show . org-narrow-to-subtree))

  :init
  (progn
    ;; Org file directories must be defined at `:init' block
    ;; so that they are visible to the navigation functions,
    ;; such as `zc-org/goto-agenda-file-heading'.
    (setq org-directory          zc-org/directory
          org-attach-directory   (f-join org-directory "data")
          org-agenda-diary-file  (f-join org-directory "diary.org")
          org-default-notes-file (f-join org-directory "notes.org")
          org-default-todos-file (f-join org-directory "todos.org")
          org-work-notes-file    (f-join org-directory "work/notes.org")
          org-work-todos-file    (f-join org-directory "work/todos.org")
          org-agenda-files       (zc-org/file-with-exts '("org"))))

  :config
  (progn
    (setq org-M-RET-may-split-line nil
          org-blank-before-new-entry '((heading . always)
                                       (plain-list-item . nil))
          org-catch-invisible-edits 'smart
          org-enforce-todo-dependencies t
          org-indirect-buffer-display 'current-window
          org-insert-heading-respect-content t
          org-agenda-restore-windows-after-quit t
          org-agenda-window-setup 'reorganize-frame
          org-src-window-setup 'current-window
          org-imenu-depth 3
          org-refile-targets '((nil :maxlevel . 3)
                               (org-agenda-files :maxlevel . 3)))

    (setq org-eldoc-breadcrumb-separator " → "
          org-image-actual-width nil
          org-pretty-entities t
          org-tags-column 0
          org-use-sub-superscripts '{})

    (setq  org-todo-keywords
           '((type "TODO(t)" "MAYBE(m)" "|" "DONE(d)")
             (type "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")
             (type "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")))

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

    (setq org-confirm-babel-evaluate #'zc-org/babel-confirm-evaluate)

    ;; Activate babel source code blocks
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (sql        . t)))

    ;; Narrow to headline after jump, which affects:
    ;; - `counsel-org-goto'
    ;; - `counsel-org-goto-all'
    (advice-add 'org-goto-marker-or-bmk :after #'zc-org/narrow-after-jump)

    ;; Instruct `smartparens' not to impose itself in org-mode
    ;; make delimiter auto-closing a little more conservative
    (with-eval-after-load 'smartparens
      (sp-with-modes 'org-mode
        (sp-local-pair "*" nil :unless '(:add sp-point-before-word-p
                                              zc-org/sp-point-at-bol-p))
        (sp-local-pair "_" nil :unless '(:add sp-point-before-word-p))
        (sp-local-pair "/" nil :unless '(:add sp-point-before-word-p
                                              zc-org/sp-point-in-checkbox-p))
        (sp-local-pair "~" nil :unless '(:add sp-point-before-word-p))
        (sp-local-pair "=" nil :unless '(:add sp-point-before-word-p))))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Org Agenda*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.5)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Org Src")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.3)))))


;; https://orgmode.org/manual/Easy-templates.html
;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates

(use-package org-tempo
  :after org

  :preface
  (defun zc-org/post-org-tempo-add-templates ()
    (mapc #'(lambda (entry)
              (let* ((key (cdr entry))
                     (value (symbol-value key)))
                (set key (-map-when 'stringp 'upcase value))))
          org-tempo-tags)
    (message "HACK: Org complete templates with uppercase keycords."))

  :config
  ;; Complete src block templates with uppercased keywords
  (advice-add 'org-tempo-add-templates
              :after #'zc-org/post-org-tempo-add-templates))



(zc-hydra/major-mode-define org-mode
  ("Basic"
   (("?" org-info "org info"))

   "Edit & Execute"
   (("ee" org-edit-special "edit")
    ("ep" org-property-action "edit property")
    ("et" counsel-org-tag "edit tag")
    ("ea" org-babel-insert-header-arg "header arg")
    ("ei" org-babel-view-src-block-info "block info")
    ("eo" org-babel-open-src-block-result "open result")
    ("ec" org-babel-remove-result-one-or-many "clear result")
    ("eC" zc-org/babel-remove-result-all "clear all result"))

   "Refactor"
   (("rs" org-sort "sort")
    ("rw" org-refile "refile"))

   "Toggle"
   (("ti" org-toggle-item "item")
    ("th" org-toggle-heading "heading"))))

(zc-hydra/major-mode-define org-agenda-mode
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
    ("gt" org-agenda-show-tags "show tags"))
   ))



(provide 'zc-org)
