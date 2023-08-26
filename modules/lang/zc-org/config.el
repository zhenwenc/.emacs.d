;;; lang/zc-org/config.el -*- lexical-binding: t; -*-

;; Override doom-emacs default key bindings
(map! :after org :map org-mode-map
      :localleader
      (:prefix ("b" . "babel")
       :desc "info"           :n "i" #'org-babel-view-src-block-info
       :desc "expand"         :n "v" #'org-babel-expand-src-block
       :desc "open"           :n "o" #'org-babel-open-src-block-result
       :desc "clare result"   :n "c" #'org-babel-remove-result-one-or-many
       :desc "clare result*"  :n "C" #'zc-org/babel-remove-result-all)

      (:prefix ("e" . "edit")
       :desc "edit"           :n "e" #'org-edit-special
       :desc "kill"           :n "d" #'org-cut-special
       :desc "yank"           :n "y" #'org-copy-special
       :desc "paste"          :n "Y" #'org-paste-special
       :desc "set properties" :n "p" #'org-property-action
       :desc "set tags"       :n "t" #'org-set-tags-command
       :desc "add note"       :n "n" #'org-add-note)

      (:prefix ("t" . "toggle")
       :desc "heading"        :n "h" #'org-toggle-heading
       :desc "item"           :n "i" #'org-toggle-item
       :desc "line display"   :n "l" #'org-toggle-link-display
       :desc "appear"         :n "p" #'org-appear-mode
       :desc "indent"         :n "I" #'org-indent-mode
       :desc "show blocks"    :n "b" #'org-show-block-all
       :desc "hide blocks"    :n "B" #'org-hide-block-all))

(after! org
  ;; Override default
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
        ;; Fold all contents on opening a org file.
        org-startup-folded t
        ;; Enable alphabetical bullet lists.
        org-list-allow-alphabetical t
        ;; Reduce search results.
        org-imenu-depth 3

        ;; Move inbox entries to the its category.
        ;;
        ;; It simply converts each rule to a regex expression, and search the
        ;; file content using `re-search-forward'.
        ;;
        ;; See `org-refile-get-targets' for details.
        org-refile-targets
        `(
          (nil :maxlevel . 1)
          (,(zc-org/file-with-exts :dir zc-org/directory) :maxlevel . 1)
          (,(zc-org/file-with-exts :dir zc-org/main-notes-dir)
           :regexp . ,(rx "* " (or "Inbox" "Archive")))
          (,(zc-org/file-with-exts :dir zc-org/work-notes-dir)
           :regexp . ,(rx "* " (or "Inbox" "Archive"))))
        org-refile-use-outline-path 'file

        ;; Our `completion-styles' does not support hierarchical steps.
        org-outline-path-complete-in-steps nil)

  ;; Appearance
  (setq org-eldoc-breadcrumb-separator " → "
        org-ellipsis (if (char-displayable-p ?) " ▼" nil))

  (unless (doom-module-p 'ui 'ligatures)
    (setq-hook! org-mode prettify-symbols-alist '(("#+BEGIN_SRC"   . "»")
                                                  ("#+begin_src"   . "»")
                                                  ("#+END_SRC"     . "«")
                                                  ("#+end_src"     . "«")
                                                  ("#+BEGIN_QUOTE" . "“")
                                                  ("#+begin_quote" . "“")
                                                  ("#+END_QUOTE"   . "“")
                                                  ("#+end_quote"   . "“")))
    (add-hook! org-mode #'prettify-symbols-mode))

  ;; Babel
  (setq org-src-window-setup 'current-window
        org-edit-src-content-indentation 0
        ;; This cause TAB on src block behaves quite weird.
        org-src-tab-acts-natively nil)

  ;; Ask for confirmation before executing src block.
  (defun zc-org/babel-confirm-evaluate (lang &rest _)
    ;; If `EVAL_NO_CONFIRM: yes' is defined
    (pcase (org-entry-get (point) "eval_no_confirm" t)
      ("yes" nil)
      ((or "no" `nil) (string-equal lang "shell"))
      (_ (error "Invalid 'EVAL_NO_CONFIRM' value, use yes/no"))))
  (setq org-confirm-babel-evaluate #'zc-org/babel-confirm-evaluate)

  ;; Inhibit displaying message in echo-area while resolving src-block info.
  ;; This is annoying when executing a code block which extracts remote params
  ;; from other code blocks, where the intermediate results will be printed to
  ;; the echo-area that causes flickering effect.
  (defun zc-org/inhibit-message (orig-fn &rest args)
    (let ((inhibit-message t)) (message (make-string 60 ?-)) (apply orig-fn args)))
  (advice-add 'org-babel-get-src-block-info      :around #'zc-org/inhibit-message)
  (advice-add 'org-babel-expand-noweb-references :around #'zc-org/inhibit-message)
  (advice-add 'org-babel-execute:restclient      :around #'zc-org/inhibit-message)

  ;; Override shell execution command
  (after! ob-shell
    (advice-add 'org-babel-execute:shell :around #'zc/org-babel-execute:shell)))
