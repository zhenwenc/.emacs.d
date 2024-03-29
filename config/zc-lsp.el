(eval-when-compile
  (require 'el-patch)
  (require 'use-package))

(require 'zc-lsp-funcs)


;; TODO Missing features
;; - TS organize import code action
;; - TS import module suggestion

(use-package lsp-bridge
  :disabled t
  :straight (:host github
             :repo "manateelazycat/lsp-bridge"
             :files ("*.el" "*.py" "*.json" "core" "acm" "langserver" "multiserver"))
  :after (evil yasnippet)

  :hook
  (python-mode     . lsp-bridge-mode)
  ;; (typescript-mode . lsp-bridge-mode)

  :general
  (:keymaps 'lsp-bridge-mode-map
   [remap xref-pop-marker-stack] #'lsp-bridge-find-def-return
   [remap xref-find-definitions] #'lsp-bridge-find-def
   [remap xref-find-definitions-other-window] #'lsp-bridge-find-def-other-window)

  (:keymaps 'acm-mode-map
   "C-j" #'acm-select-next
   "C-k" #'acm-select-prev)

  :hydra
  ((:mode (python-mode))
   ("Server"
    (("ns" lsp-bridge-restart-process              "restart")
     ("nS" lsp-bridge-kill-process                 "shutdown"))

    "Edit & Diagnostic"
    (
     ("ec" lsp-bridge-diagnostic-copy              "diagnostic copy")
     ("ei" lsp-bridge-diagnostic-ignore            "diagnostic ignore")
     ("en" lsp-bridge-diagnostic-jump-next         "diagnostic next")
     ("eN" lsp-bridge-diagnostic-jump-prev         "diagnostic prev")
     ("el" lsp-bridge-diagnostic-list              "diagnostic errors"))

    "Docs"
    (("hh" lsp-bridge-popup-documentation          "show docs")
     ("hu" lsp-bridge-find-references              "show refs")
     ("hs" lsp-bridge-signature-help-fetch         "show signture help"))

    "Refactor"
    (("rr" lsp-bridge-rename                       "rename")
     ("rf" lsp-bridge-code-format                  "format")
     ("ra" lsp-bridge-code-action                  "action"))))

  :config
  ;; Output server logs to `*lsp-bridge*' buffer, required restarting the process
  (setq lsp-bridge-enable-log nil)
  ;; Show tooltip when cursor under a diagnostic overlay
  (setq lsp-bridge-enable-hover-diagnostic t)
  ;; Do not display documentation by default, press `M-d' when needed
  (setq acm-enable-doc nil)

  ;; Evil initial state
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)

  ;; HACK Do not run post-command hook on leader key commands, such as hydra menu
  (defun zc/lsp-bridge-monitor-post-command (orig-fn &rest args)
    (let ((this-command-string (format "%s" this-command)))
      (unless (or (s-contains-p "zc-main-hydra"    this-command-string)
                  (s-contains-p "major-mode-hydra" this-command-string))
        (apply orig-fn args))))
  (advice-add #'lsp-bridge-monitor-post-command :around #'zc/lsp-bridge-monitor-post-command)

  ;; Disable the other completion plugins. lsp-bridge provides a full set from
  ;; completion backend, completion frontend to multi-backend integration solution
  (company-mode -1))



(use-package lsp-mode
  :disabled t
  :straight t
  :commands (lsp lsp-deferred)

  :hydra
  ((:mode (python-mode scala-mode rustic-mode yaml-mode))
   ("Server"
    (("ns" zc-lsp/workspace-maybe-restart  "restart")
     ("nS" lsp-workspace-shutdown          "shutdown")
     ("na" lsp-workspace-folders-add       "add folders")
     ("nx" lsp-workspace-folders-remove    "remove folders")
     ("no" lsp-workspace-folders-switch    "switch folders")
     ("nv" lsp-doctor                      "show doctor")
     ("ni" lsp-describe-session            "show sessions")
     ("nl" lsp-switch-to-io-log-buffer     "show workspace log")
     ("nL" zc-lsp/switch-to-std-log-buffer "show std log"))

    "Navigation"
    (("js" lsp-ui-find-workspace-symbol    "find symbol"))

    "Docs"
    (("hi" lsp-ui-imenu                    "show imenu")
     ("hu" lsp-ui-peek-find-references     "show references")
     ("hS" lsp-treemacs-symbols            "treemacs symbols")
     ("hs" lsp-ui-sideline-mode            "sideline" :toggle t)
     ("hl" lsp-lens-mode                   "lenses"   :toggle t)
     ("hd" zc-lsp/toggle-lsp-ui-doc-mode   "doc"      :toggle (bound-and-true-p lsp-ui-doc-mode))
     ("hh" lsp-ui-doc-glance               "doc at point"))

    "Refactor"
    (("rr" lsp-rename                      "rename")
     ("rf" lsp-format-buffer               "format")
     ("ra" zc-lsp/execute-code-action-dwim "action"))

    "Eval"
    (("ee" zc-lsp/lens-command-run         "command")
     ("el" lsp-ui-flycheck-list            "show errors"))))

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

  :init
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  :config
  (setq lsp-trace nil
        lsp-log-io t
        lsp-print-performance nil

        lsp-auto-guess-root t
        lsp-session-file (concat paths-cache-dir ".lsp-session-v1")

        ;; Declare LSP clients we might use, hmm...
        lsp-client-packages '(lsp-clients lsp-rust lsp-metals lsp-pyright lsp-yaml)

        ;; The client may send a cancel event, but most LSP
        ;; servers seems doesn't care about it at all! :P
        lsp-response-timeout 10

        ;; Enlarge file watch threshold for metals
        lsp-file-watch-threshold 1500

        ;; Ensure to respect the server recommented sync
        ;; method, otherwise it may cause issues!
        lsp-document-sync-method nil

        ;; The hover message is very distracting and unuseful
        ;; most of the time.
        ;;
        ;; NOTE: This completely disabled the hover document!
        lsp-eldoc-enable-hover nil

        ;; Show only the currently active signature, hide any
        ;; overloaded function signatures.
        lsp-eldoc-prefer-signature-help nil

        ;; Show only function signature on eldoc area, exclude
        ;; the documentation markup content.
        lsp-signature-render-documentation nil

        ;; LSP does some opinionated settings, which can be
        ;; incompatible with my config, such as how to set the
        ;; company backends.
        ;;
        ;; However, its been super painful to fixup those tons
        ;; of breaking changes on every upgrade.
        lsp-auto-configure t

        ;; Haven't have chance to discover this feature.
        ;; Possible alternative: `evil-matchit'.
        lsp-enable-folding nil

        ;; Prefer flycheck for on-the-fly syntax checking.
        lsp-diagnostic-package :flycheck

        ;; Reduce the overhead of unnecessary diagnostics in
        ;; buffer. This also brutally manipulate my customized
        ;; `flycheck-check-syntax-automatically' value.
        lsp-flycheck-live-reporting nil

        ;; Not needed, doing it myself.
        lsp-enable-snippet nil

        ;; Use recommended `company-capf' backend.
        lsp-prefer-capf t

        ;; Regain ownership of company. My pressures!
        ;;
        ;; [2020-05-13] This hack is no longer needed.
        lsp-enable-completion-at-point t

        ;; Prefer `highlight-thing'.
        lsp-enable-symbol-highlighting nil)

  ;; Customize `lsp-lsp-flycheck-warning-unnecessary-face'.
  ;;
  ;; This variable controls the values of LSP flycheck faces
  ;; which is dynamically declared.
  (with-eval-after-load 'lsp-diagnostics
    (setf (cdr (assoc 'unnecessary lsp-diagnostics-attributes))
          (list :background nil)))

  ;; Load LSP client by the current major mode, hmm..
  (-each lsp-client-packages (-rpartial #'require nil t))

  ;; Debounce `lsp-on-change' send changes to LSP server.
  ;; (defvar lsp-on-touch-time 0)
  ;; (defadvice lsp-on-change (around zc/lsp-on-change-hack activate)
  ;;   (when (> (- (float-time (current-time)) lsp-on-touch-time) 30) ;; seconds
  ;;     (setq lsp-on-touch-time (float-time (current-time)))
  ;;     ad-do-it))

  ;; Enhance with language specific features.
  (advice-add 'lsp--symbol-filter        :around #'zc-lsp/imenu-symbol-filter)
  (advice-add 'lsp--imenu-filter-symbols :around #'zc-lsp/imenu-filter-symbols)
  (advice-add 'lsp--suggest-project-root :around #'zc-lsp/infer-project-root))



(use-package lsp-ui
  :disabled t
  :straight t
  :after lsp-mode

  :general
  (:keymaps 'lsp-ui-mode-map
   ;; Prefer `lsp-mode' functions, `lsp-ui-peek-find-definitions' causes
   ;; undefined behaviour occasionally.
   ;; [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
   ;; [remap xref-pop-marker-stack] #'lsp-ui-peek-jump-backward
   [remap xref-find-definitions] #'lsp-find-definition)

  (:keymaps 'lsp-ui-peek-mode-map
   "j"   #'lsp-ui-peek--select-next
   "k"   #'lsp-ui-peek--select-prev
   "C-j" #'lsp-ui-peek--select-next
   "C-k" #'lsp-ui-peek--select-prev)

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

  :config/el-patch
  (defun lsp-ui-flycheck-list ()
    "List all the diagnostics in the whole workspace."
    (interactive)
    (let ((buffer (get-buffer-create "*lsp-diagnostics*"))
          (workspace lsp--cur-workspace)
          (window (selected-window)))
      (with-current-buffer buffer
        (lsp-ui-flycheck-list--update window workspace))
      (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-flycheck-list--refresh nil t)
      (setq lsp-ui-flycheck-list--buffer buffer)
      (el-patch-swap
        (let ((win (display-buffer-in-side-window
                    buffer `((side . ,lsp-ui-flycheck-list-position) (slot . 5) (window-width . 0.20)))))
          (set-window-dedicated-p win t)
          (select-window win)
          (fit-window-to-buffer nil nil 10))
        (select-window (display-buffer buffer)))))

  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
        lsp-ui-doc-border (doom-color 'base3)
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 30
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-delay 0.5

        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-live-reporting t
        lsp-ui-flycheck-list-position 'right

        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-code-actions-prefix (if (display-graphic-p) "" "")

        lsp-ui-imenu-kind-position 'top
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))

        lsp-ui-peek-enable nil
        lsp-ui-peek-list-width 50
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-fontify 'on-demand)

  ;; FIXME: This is kinda risky, if there is performance
  ;; issue, maybe we can workaround the racing condition.
  (setq-local flycheck-checker-error-threshold nil))



(use-package company-lsp
  :disabled t ;; replace with company-capf
  :straight t
  :after lsp-mode
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
  :disabled t ;; never use this yet
  :straight t
  :after lsp-mode
  :config
  (require 'dap-hydra)
  (setq dap-breakpoints-file (concat paths-cache-dir "dap-breakpoints"))
  (dap-mode 1)
  (dap-ui-mode 1))



(provide 'zc-lsp)
