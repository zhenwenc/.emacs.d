(eval-when-compile
  (require 'el-patch)
  (require 'use-package))

(require 'zc-lsp-funcs)



(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)

  :hydra
  ((:mode (typescript-mode python-mode scala-mode rustic-mode))
   ("Server"
    (("ns" zc-lsp/workspace-maybe-restart  "restart")
     ("nS" lsp-workspace-shutdown          "shutdown")
     ("na" lsp-workspace-folders-add       "add folders")
     ("nx" lsp-workspace-folders-remove    "remove folders")
     ("no" lsp-workspace-folders-switch    "switch folders")
     ("ni" lsp-describe-session            "show sessions")
     ("nl" lsp-switch-to-io-log-buffer     "show workspace log")
     ("nL" zc-lsp/switch-to-std-log-buffer "show log"))

    "Docs"
    (("hi" lsp-ui-imenu                    "toggle imenu")
     ("hs" lsp-ui-sideline-mode            "toggle sideline")
     ("hl" lsp-lens-mode                   "toggle lenses")
     ("hd" zc-lsp/toggle-lsp-ui-doc-mode   "toggle doc")
     ("hh" lsp-describe-thing-at-point     "doc at point")
     ("hu" lsp-ui-peek-find-references     "show references"))

    "Eval & Test"
    (("ee" zc-lsp/lens-command-run         "command"))

    "Refactor"
    (("rr" lsp-rename                      "rename")
     ("rf" lsp-format-buffer               "format")
     ("ra" zc-lsp/execute-code-action-dwim "action"))))

  :preface
  (defun zc-lsp/setup-after-open ()
    "Function for `lsp-after-open-hook' to setup the opened
new file with LSP support."
    (when (lsp--capability "documentSymbolProvider")
      (lsp-enable-imenu)))

  :preface
  (defun zc-lsp/inhibit-restart-prompt ()
    "Don't prompt to restart LSP servers while quitting Emacs."
    (setq lsp-restart 'ignore))

  :hook ((lsp-after-open . zc-lsp/setup-after-open)
         (kill-emacs     . zc-lsp/inhibit-restart-prompt))

  :config
  ;; Load LSP clients, hmm..
  (require 'lsp-clients)

  (setq lsp-trace nil
        lsp-log-io nil
        lsp-print-performance nil

        lsp-auto-guess-root t
        lsp-session-file (concat paths-cache-dir ".lsp-session-v1")

        ;; The client may send a cancel event, but most LSP
        ;; servers seems doesn't care about it at all! :P
        lsp-response-timeout 10

        ;; Ensure to respect the server recommented sync
        ;; method, otherwise it may cause issues!
        lsp-document-sync-method nil

        ;; Show only the currently active signature, hide any
        ;; overloaded function signatures.
        lsp-eldoc-prefer-signature-help nil

        ;; LSP does some opinionated settings, which can be
        ;; incompatible with my config, such as how to set
        ;; the company backends.
        lsp-auto-configure nil ; No Magic!

        lsp-prefer-flymake nil
        lsp-enable-completion-at-point nil
        lsp-enable-symbol-highlighting nil)

  ;; Enhance with language specific features
  (advice-add 'lsp--symbol-filter :around #'zc-lsp/imenu-symbol-filter)
  (advice-add 'lsp--imenu-filter-symbols :around #'zc-lsp/imenu-filter-symbols)
  (advice-add 'lsp--suggest-project-root :around #'zc-lsp/infer-project-root))

(use-package lsp-ui
  :straight t
  :after lsp

  :general
  (:keymaps 'lsp-ui-mode-map
   [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
   [remap xref-pop-marker-stack] #'lsp-ui-peek-jump-backward)

  (:keymaps 'lsp-ui-peek-mode-map
   "j"   #'lsp-ui-peek--select-next
   "k"   #'lsp-ui-peek--select-prev
   "C-j" #'lsp-ui-peek--select-next
   "C-k" #'lsp-ui-peek--select-prev)

  :preface
  (defun zc-lsp/toggle-lsp-ui-doc-mode ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc-hide)
          (message "Disabled LSP Document!"))
      (lsp-ui-doc-mode +1)
      (message "Enabled LSP Document!")))

  :preface
  (defun zc-lsp/execute-code-action-dwim ()
    (interactive)
    (cond ((lsp-code-actions-at-point)
           (call-interactively #'lsp-execute-code-action))
          (lsp-ui-sideline-mode
           (lsp-ui-sideline-apply-code-actions))
          (t (user-error "No code action available"))))

  :hook (lsp-mode . lsp-ui-mode)

  :custom-face
  (lsp-ui-doc-header     ((t (:weight     semi-bold
                              :background ,(doom-color 'bg)))))
  (lsp-ui-doc-background ((t (:background ,(doom-color 'bg)))))
  (lsp-ui-peek-header    ((t (:background ,(doom-color 'violet)
                              :foreground ,(doom-color 'bg)))))
  (lsp-ui-peek-footer    ((t (:background ,(doom-color 'violet)))))
  (lsp-ui-peek-peek      ((t (:background ,(doom-color 'base3)))))
  (lsp-ui-peek-highlight ((t (:background ,(doom-color 'yellow)
                              :foreground nil :bold t :box nil))))

  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-border (doom-color 'base3)
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 30
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-use-childframe t

        lsp-ui-flycheck-live-reporting t
        lsp-ui-flycheck-enable t

        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-code-actions-prefix (if (display-graphic-p) "" "")

        lsp-ui-imenu-kind-position 'top
        lsp-ui-imenu-colors (list (doom-color 'blue) (doom-color 'yellow))

        lsp-ui-peek-enable nil
        lsp-ui-peek-list-width 50
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-fontify 'on-demand)

  ;; FIXME: This is kinda risky, if there is performance
  ;; issue, maybe we can workaround the racing condition.
  (setq-local flycheck-checker-error-threshold nil))

(use-package company-lsp
  :straight t
  :after lsp
  :config/el-patch
  ;; HACK: Fix error when starting completion on TS annotation.
  (defun company-lsp--candidate-filter-text (candidate)
    "Return filter string of CANDIDATE.

CANDIDATE is a string created by `company-lsp--make-candidate'.
If the CompletionItem of CANDIDATE has filterText field, return
the value of filterText. Otherwise return CANDIDATE itself."
    (el-patch-let
        (($vars ((candidate-item (company-lsp--candidate-item candidate))
                 (filter-text (gethash "filterText" candidate-item)))))
      (el-patch-swap
        (let* $vars
          (or filter-text candidate))
        (-if-let* $vars
            filter-text
          (or candidate "")))))
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-async t
        company-lsp-enable-snippet t
        company-lsp-enable-recompletion t
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix)

  ;; Enhance with language specific features
  (advice-add 'company-lsp :around #'zc-lsp/company-lsp))

(use-package dap-mode
  :straight t
  :after lsp
  :config
  (require 'dap-hydra)
  (dap-mode 1)
  (dap-ui-mode 1))



(provide 'zc-lsp)
