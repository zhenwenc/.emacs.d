(eval-when-compile
  (require 'use-package)
  (require 'el-patch))



(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)

  :preface
  (defmacro zc-lsp/hydra-build-section (name prefix heads)
    (declare (indent defun))
    (let ((fname (intern (format "zc-lsp/hydra-section--%s" name))))
      `(cl-defun ,fname (&optional body rheads)
         (declare (indent defun))
         (let ((prefix (or (plist-get body :prefix) ,prefix))
               (-compare-fn (lambda (a b) (equal (car a) (car b)))))
           (mapcar (-lambda ((k . v)) (cons (concat prefix k) v))
                   (-union '(,@heads) rheads))))))

  :preface
  (defun zc-lsp/imenu-symbol-filter (orig-fn sym)
    "Advice `lsp--symbol-filter' with language specific
  filters."
    (or (funcall orig-fn sym)
        (when (derived-mode-p 'typescript-mode)
          (not (zc-typescript/lsp-symbol-filter sym)))))

  :preface
  (defun zc-lsp/imenu-filter-symbols (orig-fn symbols)
    "Advice `lsp--imenu-filter-symbols' with language
  specific filters."
    (--> (funcall orig-fn symbols)
         (cond
          ((derived-mode-p 'typescript-mode)
           (zc-typescript/lsp-filter-symbols it))
          (t it))))

  :preface
  (zc-lsp/hydra-build-section "server" "n"
    (("s" lsp-workspace-restart           "restart")
     ("S" lsp-workspace-shutdown          "shutdown")
     ("a" lsp-workspace-folders-add       "add folders")
     ("x" lsp-workspace-folders-remove    "remove folders")
     ("o" lsp-workspace-folders-switch    "switch folders")
     ("i" lsp-describe-session            "show sessions")
     ("l" lsp-switch-to-io-log-buffer     "show log")))

  :preface
  (zc-lsp/hydra-build-section "help" "h"
    (("i" lsp-ui-imenu                    "toggle imenu")
     ("s" lsp-ui-sideline-mode            "toggle sideline")
     ("l" lsp-lens-mode                   "toggle lenses")
     ("d" zc-lsp/toggle-lsp-ui-doc-mode   "toggle doc")
     ("h" lsp-describe-thing-at-point     "doc at point")
     ("u" lsp-ui-peek-find-references     "show references")))

  :preface
  (zc-lsp/hydra-build-section "refactor" "r"
    (("r" lsp-rename                      "rename")
     ("f" lsp-format-buffer               "format")
     ("a" zc-lsp/execute-code-action-dwim "action")))

  :config
  (setq lsp-trace nil
        lsp-log-io nil
        lsp-print-performance nil


        lsp-auto-guess-root t
        lsp-response-timeout 10
        lsp-document-sync-method 'incremental
        lsp-session-file (concat paths-cache-dir ".lsp-session-v1")

        ;; Show only the currently active signature, hide any
        ;; overloaded function signatures.
        lsp-eldoc-prefer-signature-help nil

        lsp-auto-configure nil ; No magic!
        lsp-prefer-flymake nil
        lsp-enable-completion-at-point nil
        lsp-enable-symbol-highlighting nil)

  (require 'lsp-clients)
  (advice-add 'lsp--symbol-filter :around #'zc-lsp/imenu-symbol-filter)
  (advice-add 'lsp--imenu-filter-symbols :around #'zc-lsp/imenu-filter-symbols))

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
  (defun zc-lsp/setup ()
    "Function for `lsp-after-open-hook' to setup the opened
new file with LSP support."
    (when (lsp--capability "documentSymbolProvider")
      (lsp-enable-imenu)))

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

  :hook ((lsp-mode       . lsp-ui-mode)
         (lsp-after-open . zc-lsp/setup))

  :custom-face
  (lsp-ui-doc-header     ((t (:background ,(doom-color 'blue)
                              :foreground ,(doom-color 'bg)))))
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
        lsp-ui-doc-border (doom-color 'blue)
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 30
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-use-childframe t

        ;; Flycheck is fast enough to report errors after every
        ;; change, but any unbalanced bracket can blow up the UI.
        ;;
        ;; However, there is a racing condition after disabling
        ;; live reporting and only trigger flycheck update on
        ;; buffer save. Sucks!
        ;;
        ;; Potential solution:
        ;;
        ;; Compare buffer last modified timestamp when
        ;; `lsp-after-diagnostics-hook', had been triggered.
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
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix))

(use-package dap-mode
  :straight t
  :after lsp
  :config
  (require 'dap-hydra)
  (dap-mode 1)
  (dap-ui-mode 1))



(provide 'zc-lsp)
