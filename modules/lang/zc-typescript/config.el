;;; lang/zc-typescript/config.el -*- lexical-binding: t; -*-



(use-package! typescript-ts-mode ; 29.1+ only
  :unless (< emacs-major-version 29)

  :mode "\\.ts\\'"
  :mode ("\\.[tj]sx\\'" . tsx-ts-mode)

  :init
  (set-tree-sitter! 'typescript-mode 'typescript-ts-mode
    '((typescript :url "https://github.com/tree-sitter/tree-sitter-typescript"
       :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
       :source-dir "typescript/src")))
  (set-tree-sitter! nil 'tsx-ts-mode
    '((tsx :url "https://github.com/tree-sitter/tree-sitter-typescript"
       :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
       :source-dir "tsx/src")))

  :config
  (dolist (mode '(typescript-ts-mode tsx-ts-mode))
    (set-electric! mode :chars '(?\} ?\) ?. ?:))
    (set-electric! mode :chars '(?\} ?\)) :words '("||" "&&"))

    ;; Enable format +onsave for source code files only
    ;;
    ;; Not working? check `(executable-find "prettier")' and try `npm i -g prettier'
    (add-hook! 'typescript-ts-mode-hook
      (defun zc-typescript/maybe-enable-formatter ()
        (unless (and buffer-file-name ;; maybe scratch or indirect buffer
                     (or (file-remote-p buffer-file-name)
                         ;; (f-ext-p buffer-file-name "js")   ; JS are shit!
                         ;; (f-ext-p buffer-file-name "jsx")  ; JS are shit!
                         (s-contains-p "/node_modules/" buffer-file-name)))
          (apheleia-mode))))

    (when (modulep! +lsp)
      (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))))



(use-package! typescript-mode
  :when (< emacs-major-version 29)

  :mode "\\.es6\\'"
  :mode "\\.cjs\\'"
  :mode "\\.mjs\\'"
  :mode "\\.jsx?\\'"
  :mode "\\.tsx?\\'"

  :interpreter (("node" . typescript-mode))

  :hook (typescript-mode . zc-typescript/add-node-modules-bin-to-path)
  :hook (typescript-mode . zc-typescript/disable-flycheck-linters)
  :hook (typescript-mode . zc-typescript/disable-flycheck-for-flow)
  :hook (typescript-mode . zc-typescript/disable-flycheck-for-node-modules)

  :config
  (setq typescript-indent-level 2)

  ;; Enable tsserver logs
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

  ;; HACK Fixes comment continuation on newline
  ;;
  ;; Doom defines `+default-open-doc-comments-block' for smartparens that added
  ;; local pair for ~/* */~.
  ;;
  ;; It also defines `+default--newline-indent-and-continue-comments-a' that
  ;; hacks `newline-and-indent' to support continue comments.
  ;;
  ;; And also defines `+evil--insert-newline-above-and-respect-comments-a' that
  ;; hacks hacks `evil-open-above' and `evil-open-below' to continue comments.
  ;;
  ;; However, they don't work well with each other :sweat:
  ;;
  (autoload 'js2-line-break "js2-mode" nil t)
  (setq-hook! 'typescript-mode-hook comment-line-break-function #'js2-line-break)
  ;;
  ;; FIXME seems broken after this refactoring:
  ;; https://github.com/doomemacs/doomemacs/commit/e43d575cafd957cdc9de4eb8518f654a5a5487c5
  (setq-hook! 'typescript-mode-hook +evil-want-o/O-to-continue-comments t)

  (after! smartparens
    ;; Enter > right before the slash in a self-closing tag automatically
    ;; inserts a closing tag and places point inside the element
    (map! :map typescript-mode-map :i ">" #'zc-typescript/sp-jsx-rewrap-tag)

    (sp-with-modes '(typescript-mode)
      ;; Revert definition in `+default-open-doc-comments-block'
      (sp-local-pair "/*" "*/" :actions :rem)
      ;; Enter < inserts </> to start a new JSX node
      ;; Also see `zc-typescript/sp-jsx-rewrap-tag'
      (sp-local-pair "<" ">" :post-handlers '(zc-typescript/sp-jsx-expand-tag))))

  ;; Enable auto-formatter on source code files only
  ;;
  ;; Not working? check `(executable-find "prettier")' and try `npm i -g prettier'
  (add-hook! 'typescript-mode-hook
    (defun zc-typescript/maybe-enable-formatter ()
      (unless (and buffer-file-name ;; maybe scratch or indirect buffer
                   (or (file-remote-p buffer-file-name)
                       ;; (f-ext-p buffer-file-name "js")   ; JS are shit!
                       ;; (f-ext-p buffer-file-name "jsx")  ; JS are shit!
                       (s-contains-p "/node_modules/" buffer-file-name)))
        (apheleia-mode))))

  ;; Disable the new font lock level introduced on #110
  (when (booleanp font-lock-maximum-decoration)
    (setq font-lock-maximum-decoration '((typescript-mode . 2))))
  (when (listp font-lock-maximum-decoration)
    (add-to-list 'font-lock-maximum-decoration '(typescript-mode . 2)))

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
    (add-to-list 'typescript--font-lock-keywords-3 item)))



(use-package! tide
  :after (:and (:or typescript-mode typescript-ts-mode) company flycheck)
  :hook ((typescript-mode    . zc-typescript/maybe-setup-tide)
         (typescript-ts-mode . zc-typescript/maybe-setup-tide))

  ;; :general
  ;; (:keymaps 'tide-mode-map
  ;;           [remap xref-find-definitions] #'tide-jump-to-definition
  ;;           [remap xref-pop-marker-stack] #'tide-jump-back)

  :config
  (map! :localleader
        :map typescript-mode-map

        (:prefix ("n" . "server")
         :desc "Restart server"      :n "s" #'tide-restart-server
         :desc "Shutdown server"     :n "S" #'tide-kill-server
         :desc "Shutdown all server" :n "K" #'zc-typescript/tide-stop-all-servers
         :desc "List servers"        :n "i" #'tide-list-servers
         :desc "Verify server"       :n "v" #'tide-verify-setup)

        (:prefix ("e" . "edit")
         :desc "Add jsdoc"           :n "d" #'tide-jsdoc-template
         :desc "Add tslint ignore"   :n "i" #'tide-add-tslint-disable-next-line)

        (:prefix ("h" . "docs")
         :desc "Show docs"           :n "h" #'tide-documentation-at-point
         :desc "Show references"     :n "u" #'tide-references
         :desc "Show error at point" :n "e" #'tide-error-at-point
         :desc "Show errors"         :n "E" #'tide-project-errors)

        (:prefix ("r" . "refactor")
         :desc "Rename symbol"       :n "r" #'tide-rename-symbol
         :desc "Format"              :n "f" #'tide-format
         :desc "Action"              :n "a" #'tide-refactor
         :desc "Code fix"            :n "c" #'tide-fix
         :desc "Organize imports"    :n "o" #'tide-organize-imports))

  ;; Enforce `+lookup/definition' command to only use tide backend
  ;; FIXME I need a reliable code navigation!
  (setq-hook! 'tide-mode-hook +lookup-definition-functions
              '(+lookup-xref-definitions-backend-fn))

  (setq tide-always-show-documentation t
        tide-completion-detailed nil ; has performance issue
        tide-completion-ignore-case t
        tide-completion-setup-company-backend nil
        tide-completion-enable-autoimport-suggestions nil)

  ;; TODO Improve imenu candiates
  ;; (setq tide-imenu-flatten nil)

  (set-company-backend! 'tide-mode 'company-tide)
  ;; navigation
  (set-lookup-handlers! 'tide-mode :async t
    :xref-backend #'xref-tide-xref-backend
    :documentation #'tide-documentation-at-point)
  (set-popup-rule! "^\\*tide-documentation" :quit t)

  ;; HACK: This is hacky, is there any better way?
  (advice-add 'tide-completion-doc-buffer :override #'ignore)
  (advice-add 'tide-eldoc-maybe-show  :around #'zc-typescript/tide-eldoc-maybe-show)
  (advice-add 'tide-show-project-info :around #'zc-typescript/tide-show-project-info)

  (defun zc-typescript/maybe-setup-tide ()
    ;; Format org-mode source block
    (when (bound-and-true-p org-src-mode)
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Skip when connected to remote server via TRAMP.
    ;;
    ;; TODO: Maybe use `dumb-jump' instead?
    (unless (file-remote-p default-directory)
      (tide-setup)
      ;; Eldoc is activated too soon and disables itself, thinking there is no eldoc
      ;; support in the current buffer, so we must re-enable it later once eldoc
      ;; support exists. It is set *after* tide-mode is enabled, so enabling it on
      ;; `tide-mode-hook' is too early, so...
      (eldoc-mode +1)))

  ;; HACK: Instruct `tide-buffer-file-name' to return correct file path
  ;;       when editing `org-mode' source block on indirect buffer.
  (defun zc-typescript/tide-buffer-file-name (orig-fn)
    (or (and (bound-and-true-p org-src--overlay)
             (-some->> (overlay-buffer org-src--overlay)
               (buffer-base-buffer)
               (buffer-file-name)))
        (funcall orig-fn)))
  (advice-add 'tide-buffer-file-name :around 'zc-typescript/tide-buffer-file-name)

  ;; Cleanup tsserver when no tide buffers are left
  (add-hook! 'tide-mode-hook
    (add-hook 'kill-buffer-hook #'zc-typescript/tide-cleanup-processes-h nil 'local))

  ;; HACK: Flycheck generated temporary file hammers file watchers.
  ;;       Remove the hack after these issues are fixed:
  ;; https://github.com/flycheck/flycheck/issues/1446
  ;; https://github.com/flycheck/flycheck/issues/1472
  (ignore-errors
    (setcar (memq 'source-inplace
                  (flycheck-checker-get 'typescript-tslint 'command))
            'source-original)))



(after! org-src
  (defalias 'org-babel-execute:ts 'org-babel-execute:typescript)
  (add-to-list 'org-src-lang-modes '("ts"         . typescript-ts))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts)))
