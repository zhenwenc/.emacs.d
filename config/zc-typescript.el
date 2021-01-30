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
    (kbd ">") 'zc-typescript/sp-jsx-rewrap-tag)

  ;; Integration with `org-mode'
  (with-eval-after-load 'org

    ;; Improve TypeScript source block experience
    ;; http://rwx.io/posts/org-with-babel-node-updated/
    (defun org-babel-execute:typescript (body params)
      "Execute a block of Typescript code with org-babel.
  This function is called by `org-babel-execute-src-block'."
      (let* ((ts-node-options (json-serialize '(module "CommonJS" target "ES2017")))
             (dir (or (cdr (assq :dir params)) zc-org/directory))
             (cmd (or (cdr (assq :cmd params)) (format "ts-node -T -O '%s'" ts-node-options)))
             ;; Transpile 'import' statements to 'require'
             (script-file (org-babel-temp-file "js-script-" ".ts"))
             (output-file (org-babel-temp-file "js-script-" ".js"))
             (babel-cmd (f-join zc-org/directory "node_modules/.bin/babel"))
             (babel-res (progn (with-temp-file script-file (insert body))
                               (shell-command-to-string
                                (concat babel-cmd
                                        " --no-babelrc"
                                        " --presets @babel/preset-env"
                                        " --plugins @babel/plugin-transform-runtime"
                                        " --extensions .ts"
                                        " --out-file " output-file
                                        " " script-file))))
             (babel-body (f-read output-file))
             (node-path (concat "NODE_PATH=" (f-join dir "node_modules")))
             (node-opts (format "NODE_OPTIONS='--unhandled-rejections=strict'"))
             (org-babel-js-cmd (format "%s %s %s" node-path node-opts cmd))
             (org-babel-js-function-wrapper "%s"))
        (when (s-equals? "yes" (cdr (assq :debug params)))
          (message "[DEBUG] Transpiled source code:\n\n%s\n%s" babel-res babel-body))
        (org-babel-execute:js babel-body params)))

    ;; FIXME: This doesn't seem to work
    ;; (defun org-babel-edit-prep:typescript (info)
    ;;   (let* ((dir (or (->> info caddr (alist-get :dir)) zc-org/directory))
    ;;          (default-directory dir)
    ;;          (config (tide-safe-json-read-string
    ;;                   (tide-command-to-string "node" '("tsc" "--showConfig")))))
    ;;     (message "Set tide project root to %s" dir)
    ;;     (setq-local tide-project-root (f-expand dir))
    ;;     (puthash (tide-project-name) config tide-project-configs)
    ;;     (zc-typescript/maybe-setup-tide)))

    (defalias 'org-babel-execute:ts   'org-babel-execute:typescript)
    (defalias 'org-babel-edit-prep:ts 'org-babel-edit-prep:typescript)))



(use-package tide
  :straight t
  :after (:and typescript-mode company flycheck)

  :general
  (:keymaps 'tide-mode-map
   [remap xref-find-definitions] #'tide-jump-to-definition
   [remap xref-pop-marker-stack] #'tide-jump-back)

  (:states 'motion :keymaps 'tide-references-mode-map
   "RET"       #'tide-goto-reference
   "TAB"       #'tide-cycle-next-reference
   "<backtab>" #'tide-cycle-previous-reference
   "C-j"       #'tide-find-next-reference
   "C-k"       #'tide-find-previous-reference
   "q"         #'quit-window)

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

  :hook (typescript-mode . zc-typescript/maybe-setup-tide)

  :init
  ;; HACK: This is hacky, is there any better way?
  (advice-add 'tide-completion-doc-buffer :override #'ignore)
  (advice-add 'tide-eldoc-maybe-show  :around #'zc-typescript/tide-eldoc-maybe-show)
  (advice-add 'tide-show-project-info :around #'zc-typescript/tide-show-project-info)

  :config
  (defun zc-typescript/maybe-setup-tide ()
    (interactive)
    ;; Format org-mode source block
    (when (bound-and-true-p org-src-mode)
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    (tide-setup)
    (eldoc-mode +1)
    (flycheck-mode +1))

  ;; HACK: Instruct `tide-buffer-file-name' to return correct file path
  ;;       when editing `org-mode' source block on indirect buffer.
  (defun zc-typescript/tide-buffer-file-name (orig-fn)
    (or (and (bound-and-true-p org-src--overlay)
             (-some->> (overlay-buffer org-src--overlay)
               (buffer-base-buffer)
               (buffer-file-name)))
        (funcall orig-fn)))
  (advice-add 'tide-buffer-file-name :around 'zc-typescript/tide-buffer-file-name)

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
