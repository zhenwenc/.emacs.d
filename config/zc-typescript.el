(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'zc-hydra-funcs)
(require 'zc-typescript-funcs)



(use-package typescript-mode
  :straight t
  :defer t

  :mode (("\\.es6\\'"  . typescript-mode)
         ("\\.jsx?\\'" . typescript-mode)
         ("\\.tsx?\\'" . typescript-mode))

  :interpreter (("node" . typescript-mode))

  :hook
  (find-file       . zc-typescript/set-node-modules-readonly)
  (typescript-mode . zc-typescript/add-node-modules-bin-to-path)
  (typescript-mode . zc-typescript/disable-flycheck-linters)
  (typescript-mode . zc-typescript/disable-flycheck-for-flow)
  (typescript-mode . zc-typescript/disable-flycheck-for-node-modules)

  ;; [2020-06-20] Switched back to Tide
  ;;
  ;; - The user experience (performance) with `lsp-mode' was really bad.
  ;; - All known TS/JS LSP server projects are abandoned.
  ;;
  ;; (typescript-mode . lsp-deferred)
  ;; (lsp-after-open  . zc-typescript/setup-lsp-workspace)

  :config
  (setq typescript-indent-level 2)

  ;; Disable the new font lock level introduced on #110
  (add-to-list 'font-lock-maximum-decoration '(typescript-mode . 3))

  (defconst zc-typescript/method-keyword-re
    (regexp-opt '("async" "static" "public" "private" "protected" "get" "set")))

  (defconst zc-typescript/generic-type-re
    ".*" ;; FIXME make it more specific
    "Regexp matching a typescript generic type identifier, without grouping.")

  (defconst zc-typescript/method-heading-re
    (concat
     "\\s-*" zc-typescript/method-keyword-re
     "\\s-+\\(" typescript--name-re "\\)"
     "\\(?:<" zc-typescript/generic-type-re ">\\)?"
     "("))

  (defconst zc-typescript/function-heading-re
    (concat
     "\\s-*" zc-typescript/method-keyword-re
     "\\s-+function"
     "\\s-*\\(" typescript--name-re "\\)"))

  (defconst zc-typescript/decorator-re
    (rx (and "@" (in "a-zA-Z_.") (0+ (in "a-zA-Z0-9_.")))))

  (dolist (item `((, zc-typescript/decorator-re        . font-lock-preprocessor-face)
                  (, zc-typescript/method-heading-re   1 font-lock-function-name-face)
                  (, zc-typescript/function-heading-re 1 font-lock-function-name-face)))
    (add-to-list 'typescript--font-lock-keywords-3 item))

  ;; Enter > right before the slash in a self-closing tag automatically
  ;; inserts a closing tag and places point inside the element
  (evil-define-key 'insert typescript-mode-map
    (kbd ">") 'zc-typescript/sp-jsx-rewrap-tag))



(use-package tide
  :straight t
  :after (typescript-mode company flycheck)

  :general
  (:keymaps 'tide-mode-map
   [remap xref-find-definitions] #'tide-jump-to-definition
   [remap xref-pop-marker-stack] #'tide-jump-back)

  (:states 'normal :keymaps 'tide-references-mode-map
   "RET" #'tide-goto-reference
   "p"   #'tide-find-previous-reference
   "n"   #'tide-find-next-reference
   "q"   #'quit-window)

  :hydra
  ((:mode (typescript-mode))
   ("Server"
    (("ns" tide-restart-server                     "restart")
     ("nS" tide-kill-server                        "shutdown")
     ("nK" zc-typescript/tide-stop-all-servers     "shutdown all")
     ("ni" tide-list-servers                       "servers")
     ("nv" tide-verify-setup                       "verify"))

    "Edit"
    (("ed" tide-jsdoc-template                     "jsdoc template")
     ("ei" tide-add-tslint-disable-next-line       "tslint ignore"))

    "Docs"
    (("hh" tide-documentation-at-point             "show docs")
     ("hu" tide-references                         "show refs")
     ("he" tide-error-at-point                     "show error")
     ("hE" tide-project-errors                     "show errors"))

    "Refactor"
    (("rr" tide-rename-symbol                      "rename")
     ("rf" tide-format                             "format")
     ("ra" tide-refactor                           "action")
     ("rF" tide-fix                                "code fix")
     ("ro" tide-organize-imports                   "sort imports"))))

  :preface
  (defun zc-typescript/maybe-setup-tide ()
    (interactive)
    (unless (or (not buffer-file-name)
                (f-ext? buffer-file-name))
      (setq-local tide-require-manual-setup t))
    (tide-setup)
    (eldoc-mode +1)
    (flycheck-mode +1))

  :hook (typescript-mode . zc-typescript/maybe-setup-tide)

  :init
  ;; HACK: This is hacky, is there any better way?
  (advice-add 'tide-completion-doc-buffer :override #'ignore)
  (advice-add 'tide-load-tsconfig :override #'zc-typescript/tide-load-tsconfig)
  (advice-add 'tide-eldoc-maybe-show :around #'zc-typescript/tide-eldoc-maybe-show)

  :config
  (setq tide-completion-detailed nil
        tide-completion-ignore-case t
        tide-always-show-documentation t)

  ;; HACK: Flycheck generated temporary file hammers file watchers.
  ;;       Remove the hack after these issues are fixed:
  ;; https://github.com/flycheck/flycheck/issues/1446
  ;; https://github.com/flycheck/flycheck/issues/1472
  (ignore-errors
    (setcar (memq 'source-inplace
                  (flycheck-checker-get 'typescript-tslint 'command))
            'source-original)))



(provide 'zc-typescript)
