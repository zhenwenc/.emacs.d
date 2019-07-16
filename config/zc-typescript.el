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

  ;; TODO: How to control hydra override order?
  ;; :hydra
  ;; ("Refactor"
  ;;  (("rf" prettier-js "format")))

  :preface
  (defun zc-typescript/set-node-modules-readonly ()
    (when (and (buffer-file-name)
               (s-contains-p "/node_modules/" buffer-file-name))
      (read-only-mode +1)))

  :preface
  (defun zc-typescript/add-node-modules-bin-to-path ()
    "Use binaries from node_modules, where available."
    (when-let (root (projectile-project-p))
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

  :preface
  (defun zc-typescript/disable-flycheck-linters ()
    "Linters are pretty slow, and we use Prettier anyway."
    (zc-flycheck/disable-checkers 'javascript-jshint 'typescript-tslint))

  :preface
  (defun zc-typescript/disable-flycheck-for-flow ()
    (when (and buffer-file-name
               (string= (f-ext buffer-file-name) "js")
               (save-excursion (goto-char (point-min))
                               (search-forward "@flow" nil t)))
      (zc-flycheck/disable-checkers 'typescript-tide)))

  :preface
  (defun zc-typescript/disable-flycheck-for-node-modules ()
    (when (and buffer-file-name
               (s-contains-p "/node_modules/" buffer-file-name))
      (apply 'zc-flycheck/disable-checkers
             (->> flycheck-checkers
                  (-map #'symbol-name)
                  (--filter (or (string-prefix-p "javascript" it)
                                (string-prefix-p "typescript" it)))
                  (-map #'intern)))))

  :hook
  (find-file       . zc-typescript/set-node-modules-readonly)
  (post-command    . zc-typescript/post-command-hook-handler)
  (typescript-mode . zc-typescript/add-node-modules-bin-to-path)
  (typescript-mode . zc-typescript/disable-flycheck-linters)
  (typescript-mode . zc-typescript/disable-flycheck-for-flow)
  (typescript-mode . zc-typescript/disable-flycheck-for-node-modules)
  (typescript-mode . lsp-deferred)

  :config
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
  (with-eval-after-load 'smartparens
    (sp-with-modes '(typescript-mode)
      (sp-local-pair "/*" "*/"
                     :post-handlers '(("| " "SPC")
                                      (zc-typescript/sp-comment-expand "RET")))

      ;; Enter < inserts </> to start a new JSX node
      (sp-local-pair "<" ">"
                     :post-handlers '(zc-typescript/sp-jsx-expand-tag))))

  ;; Enter > right before the slash in a self-closing tag automatically
  ;; inserts a closing tag and places point inside the element
  (evil-define-key 'insert typescript-mode-map
    (kbd ">") 'zc-typescript/sp-jsx-rewrap-tag))



(use-package tide
  :disabled ; switched to LSP
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

  :config
  (setq tide-completion-detailed nil
        tide-completion-ignore-case t)

  ;; HACK: Flycheck generated temporary file hammers file watchers.
  ;;       Remove the hack after these issues are fixed:
  ;; https://github.com/flycheck/flycheck/issues/1446
  ;; https://github.com/flycheck/flycheck/issues/1472
  (ignore-errors
    (setcar (memq 'source-inplace
                  (flycheck-checker-get 'typescript-tslint 'command))
            'source-original)))



(provide 'zc-typescript)
