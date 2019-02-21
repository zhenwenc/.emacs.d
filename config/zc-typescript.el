(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'zc-hydra-funcs)
(require 'zc-typescript-funcs)

(autoload 'f-join "f")
(autoload 'flycheck-add-mode "flycheck")
(autoload 'projectile-project-p "projectile")



(use-package typescript-mode
  :straight t
  :defer t

  :mode (("\\.es6\\'"  . typescript-mode)
         ("\\.jsx?\\'" . typescript-mode)
         ("\\.tsx?\\'" . typescript-mode))

  :interpreter (("node" . typescript-mode))

  :preface
  (defun zc-typescript/set-node-modules-readonly ()
    (when (and (buffer-file-name)
               (string-match-p (rx "/node_modules/") (buffer-file-name)))
      (read-only-mode +1)))

  :preface
  (defun zc-typescript/add-node-modules-bin-to-path ()
    "Use binaries from node_modules, where available."
    (when-let (root (projectile-project-p))
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

  :hook ((find-file . zc-typescript/set-node-modules-readonly)
         (typescript-mode . zc-typescript/add-node-modules-bin-to-path))

  :config
  (progn
    (setq typescript-indent-level 2)

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

    ;; Enhance smartparens
    (sp-with-modes '(typescript-mode)
      (sp-local-pair "/*" "*/"
                     :post-handlers
                     '(("| " "SPC") (zc-typescript/sp-comment-expand "RET")))

      ;; Enter < inserts </> to start a new JSX node
      (sp-local-pair "<" ">"
                     :post-handlers
                     '(zc-typescript/sp-jsx-expand-tag)))

    ;; Enter > right before the slash in a self-closing tag automatically
    ;; inserts a closing tag and places point inside the element
    (evil-define-key 'insert typescript-mode-map
      (kbd ">") 'zc-typescript/sp-jsx-rewrap-tag)))



(use-package prettier-js
  :straight t
  :after (:any zc-web-modes typescript-mode graphql-mode)
  :commands (prettier-js prettier-js-mode)
  :hook ((graphql-mode . enable-prettier-mode)
         (typescript-mode . enable-prettier-mode)
         (zc-web-css-mode . enable-prettier-mode))
  :preface
  (defun enable-prettier-mode ()
    (if (not (string= (projectile-project-name) "apollo-codegen"))
        (prettier-js-mode)))
  :config
  ;; NOTE: If the prettier version seems outdated, check .nvmrc
  (setq prettier-js-args '("--single-quote" "--trailing-comma" "es5")))



(use-package nvm
  :disabled t ; use nodenv
  :straight t
  :functions (nvm-use-for-buffer)
  :preface
  (defun zc-typescript/maybe-use-nvm ()
    ;; NOTE: `nvm use' command doesn't update .nvmrc
    (if (locate-dominating-file default-directory ".nvmrc")
        (progn (nvm-use-for-buffer) t)
      (message "Looks like [.nvmrc] is missing!"))))



(use-package tide
  :straight t
  :after (typescript-mode company flycheck)

  :general
  (:states '(normal insert) :keymaps 'tide-mode-map
           "M-." #'tide-jump-to-definition
           "M-," #'tide-jump-back)

  (:states 'normal :keymaps 'tide-references-mode-map
           "RET" #'tide-goto-reference
           "p"   #'tide-find-previous-reference
           "n"   #'tide-find-next-reference
           "q"   #'quit-window)

  :preface
  (defun zc-typescript/maybe-setup-tide ()
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (flycheck-mode +1))

  :preface
  (defun zc-typescript/disable-flycheck-for-node-modules ()
    (when (and (buffer-file-name)
               (s-contains-p "node_modules" (buffer-file-name))
               (boundp 'flycheck-checkers)
               (boundp 'flycheck-disabled-checkers))
      (let* ((js-checkers (seq-filter
                           (lambda (checker)
                             (string-prefix-p "javascript" (symbol-name checker)))
                           flycheck-checkers))
             (updated (cl-union flycheck-disabled-checkers js-checkers)))
        (setq flycheck-disabled-checkers updated))))

  :hook ((typescript-mode . zc-typescript/maybe-setup-tide)
         (typescript-mode . zc-typescript/disable-flycheck-for-node-modules))

  :init
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*tide-documentation*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-width    . 0.5)))

    ;; HACK: This is hacky, is there any better way?
    (advice-add 'tide-completion-doc-buffer :override #'ignore))

  :config
  (progn
    ;; NOTE: Disabled tslint if slow
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'typescript-tslint)

    ;; HACK: Flycheck generated temporary file hammers file watchers.
    ;;       Remove the hack after these issues are fixed:
    ;; https://github.com/flycheck/flycheck/issues/1446
    ;; https://github.com/flycheck/flycheck/issues/1472
    (ignore-errors
      (setcar (memq 'source-inplace
                    (flycheck-checker-get 'typescript-tslint 'command))
              'source-original))))



(zc-hydra/major-mode-define typescript-mode
  ("Basic"
   (("q" nil "quit")
    ("<escape>" nil nil))

   "Server"
   (("ns" tide-restart-server "restart server")
    ("nS" zc-typescript/tide-stop-all-servers "stop all servers")
    ("nv" tide-verify-setup "verify setup"))

   "Navigation"
   (("gg" tide-jump-to-definition "goto definition")
    ("gi" tide-jump-to-implementation "goto implementation"))

   "Refactor"
   (("rr" tide-rename-symbol "rename symbol")
    ("rf" prettier-js "prettier")
    ("rx" tide-fix "fix code")
    ("rX" zc-typescript/linter-fix-file "fix file"))

   "Docs"
   (("hu" tide-references "references")
    ("hh" tide-documentation-at-point "doc at point"))))



(provide 'zc-typescript)
