(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'dash-functional)
(require 'general)
(require 'zc-org-funcs)
(require 'zc-hydra-funcs)



(defconst zc-org/directory "~/org")



(use-package org
  :straight org-plus-contrib
  :defer t

  :general
  (:keymaps 'org-src-mode-map
            "C-c C-c" #'org-edit-src-exit)

  (:states 'normal :keymaps 'org-mode-map
           "RET" #'org-return)

  (:states 'motion :keymaps 'org-agenda-mode-map
           "RET" #'org-agenda-switch-to
           "sc"  #'zc-org/agenda-filter-by-category
           "sC"  #'org-agenda-filter-by-category)

  :preface
  (defun zc-org/file-with-exts (exts)
    "Return files in `org-directory' that matches extension in EXTS."
    (f-files org-directory
             (lambda (file)
               (-contains? exts (f-ext file)))))

  :init
  (add-to-list 'auto-mode-alist '("\\.trello\\'" . org-mode))

  :config
  (progn
    (setq org-directory zc-org/directory
          org-default-notes-file (f-join org-directory "notes.org")
          org-agenda-diary-file (f-join org-directory "diary.org")
          org-agenda-files (zc-org/file-with-exts '("org" "trello"))
          org-attach-directory (f-join org-directory "data"))

    (setq org-M-RET-may-split-line nil
          org-blank-before-new-entry '((heading . always)
                                       (plain-list-item . nil))
          org-catch-invisible-edits 'smart
          org-enforce-todo-dependencies t
          org-pretty-entities t
          org-indirect-buffer-display 'current-window
          org-insert-heading-respect-content t
          org-agenda-restore-windows-after-quit t
          org-agenda-window-setup 'reorganize-frame
          org-src-window-setup 'current-window)

    (setq  org-todo-keywords
           '((type "TODO(t)" "NEXT(n)" "MAYBE(m)" "|" "DONE(d)" "CANCELLED(c)")
             (type "WAITING(w)" "MEETING(M)" "|")))

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
                   "t" "Todo" "* TODO %?\n%u\n"
                   '(file org-default-notes-file))

                  (entry
                   "T" "Todo (ask)" "* TODO %?\n%u\n"
                   '(function zc-org/read-capture-target-file))

                  (entry
                   "i" "Idea" "* MAYBE :Idea: %?\n%t"
                   '(file org-default-notes-file))

                  (entry
                   "I" "Idea (ask)" "* MAYBE :Idea: %?\n%t"
                   '(function zc-org/read-capture-target-file))

                  (entry
                   "n" "Next" "* NEXT %?\n%t"
                   '(file org-default-notes-file))

                  (entry
                   "N" "Next (ask)" "* NEXT %?\n%t"
                   '(function zc-org/read-capture-target-file))

                  (entry
                   "m" "Note" "* %?\n%t"
                   '(file org-default-notes-file))

                  (entry
                   "m" "Note (ask)" "* %?\n%t"
                   '(function zc-org/read-capture-target-file))

                  (entry
                   "d" "Diary" "* %?\n%U\n"
                   '(file+datetree org-agenda-diary-file)
                   :clock-in t :clock-resume t)

                  (entry
                   "D" "Diary (ask)" "* %?\n%U\n"
                   '(function zc-org/read-capture-target-file)
                   :clock-in t :clock-resume t)

                  (entry
                   "r" "Read later" "* MAYBE :Read: %i%?"
                   '(file+olp org-default-notes-file)))))

    ;; Activate babel source code blocks
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (sql        . t)))

    ;; Override the context sensitive C-c C-c key
    (add-hook 'org-ctrl-c-ctrl-c-hook 'zc-org/ctrl-c-ctrl-c-hook)

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
  :after org)



(eval-and-compile
  (defconst zc-org/trello-load-path
    (expand-file-name "local/org-trello" user-emacs-directory)))

(use-package org-trello
  :load-path zc-org/trello-load-path
  :commands (org-trello-mode)
  :preface
  (defun zc-org/maybe-enable-trello ()
    "Enable buffer with `org-trello' if file name ends with `.trello'."
    (let ((filename (buffer-file-name (current-buffer))))
      (when (and filename (f-ext? filename "trello"))
        (org-trello-mode))))

  :hook (org-mode . zc-org/maybe-enable-trello)
  :init
  (general-setq org-trello-default-prefix-keybinding nil
                org-trello-current-prefix-keybinding nil
                org-trello-files (f-files zc-org/directory (-rpartial #'f-ext? "trello"))))



(zc-hydra/major-mode-define org-mode
  ("Basic"
   (("?" org-info "org info"))

   "Edit & Execute"
   (("ee" org-babel-execute-src-block-maybe "execute block")
    ("ep" org-property-action "edit property")
    ("ea" org-babel-insert-header-arg "edit header arg")
    ("ev" org-babel-check-src-block "source block verify")
    ("ei" org-babel-view-src-block-info "source block info")
    ("eo" org-babel-open-src-block-result "open result")
    ("ec" org-babel-remove-result-one-or-many "clear result")
    ("eC" zc-org/babel-remove-result-all "clear all result"))

   "Insert"
   (("id" org-insert-drawer "insert drawer")
    ("ih" org-insert-heading "insert heading")
    ("is" org-insert-structure-template "insert template")
    ("il" org-insert-link "insert link"))

   "Server"
   (("ns" org-trello-sync-buffer "trello sync buffer")
    ("nu" org-trello-update-board-metadata "trello update metadata")
    ("nU" org-trello-install-board-metadata "trello install metadata"))))

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
