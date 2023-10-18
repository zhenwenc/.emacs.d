;;; lang/zc-typescript/autoload.el -*- lexical-binding: t; -*-


;; Org Mode Integration

;; Dummy executor for JSON source block
;;;###autoload
(defun org-babel-execute:json (body params)
  "Execute a block of JSON code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (org-babel-result-cond (cdr (assq :result-params params)) body body))

;; Improve TypeScript source block experience
;; http://rwx.io/posts/org-with-babel-node-updated/
;;;###autoload
(defun org-babel-execute:typescript (body params)
  "Execute a block of Typescript code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'ob-js)
  (let* ((dir (or (cdr (assq :dir params)) zc-org/directory))
         (env (or (cdr (assq :env params)) ""))
         (node-path (concat "NODE_PATH=" (f-join dir "node_modules")))
         (node-opts (format "NODE_OPTIONS='--unhandled-rejections=warn --max-http-header-size=16384'"))
         (node-envs (concat "NODENV_VERSION=" (s-trim (shell-command-to-string "nodenv global"))))
         (term-name (if (s-equals? "no" (cdr (assq :color params))) "TERM=dumb" ""))
         (cmd-env (format "%s %s %s %s" node-envs node-path node-opts env))
         (cmd (if (s-equals? "yes" (cdr (assq :esm params)))
                  (zc-org-babel-execute-typescript-esm body params)
                (zc-org-babel-execute-typescript-cjs body params)))
         (org-babel-js-function-wrapper "%s"))
    ;; Execute the code block with `compilation'
    (if (s-equals? "yes" (cdr (assq :compile params)))
        ;; Do not highlight errors for arbitrary outputs
        (let ((compilation-start-hook '(lambda (&rest _ignore)
                                         (make-local-variable 'compilation-error-regexp-alist)
                                         (setq-local compilation-error-regexp-alist nil))))
          (compile (format "%s %s %s" term-name cmd-env (plist-get cmd :cmd-compile))))
      ;; Execute the code block with `org-babel-execute'
      (let* ((result (org-babel-eval (format "%s %s" cmd-env (plist-get cmd :cmd-eval)) "")))

        (org-babel-result-cond (cdr (assq :result-params params))
          result (org-babel-js-read result))))))

(defun zc-org-babel-execute-typescript-cjs (body params)
  (let* ((ts-node-opts (json-serialize '(module "CommonJS" target "ES2017" skipLibCheck t)))
         (ts-node (f-join zc-org/directory "node_modules/.bin/ts-node"))
         (cmd (or (cdr (assq :cmd params)) (format "%s -T -O '%s'" ts-node ts-node-opts)))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:js params)))
         ;; Transpile 'import' statements to 'require'
         (script-file (org-babel-temp-file "js-script-" ".ts"))
         (output-file (org-babel-temp-file "js-script-" ".js"))
         ;; Transform with Babel for older NodeJS that doesn't support morden JS module
         (babel-path (f-join zc-org/directory "node_modules"))
         (babel-res (progn (with-temp-file script-file (insert full-body))
                           (when (s-equals? "yes" (cdr (assq :babel params)))
                             (shell-command-to-string
                              (concat "NODE_PATH=" babel-path
                                      " " (f-join babel-path ".bin/babel")
                                      " --no-babelrc"
                                      " --presets @babel/preset-env,@babel/preset-typescript"
                                      " --plugins @babel/plugin-transform-runtime"
                                      " --extensions .ts"
                                      " --out-file " output-file
                                      " " script-file)))))
         (node-file (if babel-res output-file script-file)))
    ;; Print the transpiled output for debugging
    (when (s-equals? "yes" (cdr (assq :debug params)))
      (let ((inhibit-message t)) ;; skip echo area
        (message "[DEBUG] Transpiled source code:\n\n%s\n%s" babel-res (f-read node-file))))
    ;; Generate execution commands
    `(:cmd-compile ,(format "%s %s" cmd node-file)
      :cmd-eval    ,(format "%s %s" cmd (org-babel-process-file-name script-file)))))

;; https://github.com/esbuild-kit/tsx
(defun zc-org-babel-execute-typescript-esm (body params)
  (let* ((tsx (f-join zc-org/directory "node_modules/.bin/tsx"))
         (cmd (or (cdr (assq :cmd params)) (format "%s" tsx)))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:js params)))
         (script-file (org-babel-temp-file "js-script-" ".mjs")))
    (with-temp-file script-file (insert full-body))
    ;; Generate execution commands
    `(:cmd-compile ,(format "%s %s" cmd script-file)
      :cmd-eval    ,(format "%s %s" cmd (org-babel-process-file-name script-file)))))

;; (defun zc-org-babel-execute-typescript-esm (body params)
;;   (let* ((ts-node-opts (json-serialize '(module "NodeNext" moduleResolution "NodeNext" esModuleInterop t)))
;;          (ts-node (f-join zc-org/directory "node_modules/.bin/ts-node-esm --skipProject"))
;;          (cmd (or (cdr (assq :cmd params)) (format "%s -T -O '%s'" ts-node ts-node-opts)))
;;          (full-body (org-babel-expand-body:generic
;;                      body params (org-babel-variable-assignments:js params)))
;;          (script-file (org-babel-temp-file "js-script-" ".mjs")))
;;     (with-temp-file script-file (insert full-body))
;;     ;; Generate execution commands
;;     `(:cmd-compile ,(format "%s %s" cmd script-file)
;;       :cmd-eval    ,(format "%s %s" cmd (org-babel-process-file-name script-file)))))


;; Smartparens

;;;###autoload
(defun zc-typescript/sp-jsx-rewrap-tag (&rest _ignored)
  "Rewrap the self-closing JSX tag <_/> to <_>|</_> if point is followed by />."
  (interactive "P")
  (if (sp--looking-at-p "/>")
      (let ((tag (zc-typescript/sp-jsx-get-tag-name))
            (beg (save-excursion (sp-backward-whitespace))))
        (delete-region beg (re-search-forward ">"))
        (insert ">\n")
        (save-excursion
          (insert "\n</" tag ">")
          (indent-according-to-mode))
        (indent-according-to-mode))
    (self-insert-command
     (prefix-numeric-value current-prefix-arg))))

(defun zc-typescript/sp-react-buffer-p (&rest _ignored)
  "Return t if the current buffer is a TSX/JSX buffer."
  (string-match-p "jsx\\|tsx" (file-name-extension (buffer-name))))

(defun zc-typescript/sp-jsx-expand-tag (id action _context)
  "Expand JSX tag <> to self-closing form </> if point is not after a word."
  (when (and (eq action 'insert)
             (not (sp--looking-back-p
                   (concat "\\(\\sw\\|\\s_\\)" (regexp-quote id))))
             (zc-typescript/sp-react-buffer-p))
    (save-excursion (insert "/"))))

(defun zc-typescript/sp-jsx-get-tag-name (&rest _ignored)
  "Return the JSX tag name inclosed in <> pair."
  (let* ((matched (save-excursion (sp-end-of-sexp)))
         (beg (plist-get matched :beg))
         (end (plist-get matched :end))
         (str (buffer-substring beg end))
         (sub (replace-regexp-in-string "/\\|<\\|>" "" str)))
    (string-trim (car (split-string sub " ")))))


;; Hooks

;;;###autoload
(defun zc-typescript/setup-tree-sitter ()
  "Integration with `tree-sitter-hl-mode'
https://github.com/tree-sitter/tree-sitter-typescript"
  (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                (lambda (capture-name)
                  "Reduce distraction from the rainbowlic colors."
                  (not (or (string= capture-name "property")
                           (string= capture-name "method.call")
                           (string= capture-name "function.call")))))
  ;; FIXME Syntax highlighting for TSX seems incorrect
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/tree-sitter/config.el
  (when (and (buffer-file-name)
             (not (f-ext-p (buffer-file-name) "tsx")))
    (tree-sitter-hl-mode 1)))

;;;###autoload
(defun zc-typescript/setup-reduced-fontlock-level ()
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

;;;###autoload
(defun zc-typescript/add-node-modules-bin-to-path ()
  "Use binaries from node_modules, where available."
  (when-let (root (projectile-project-p))
    (make-local-variable 'exec-path)
    (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

;;;###autoload
(defun zc-typescript/disable-flycheck-linters ()
  "Linters are pretty slow, and we use Prettier anyway."
  (zc-flycheck/disable-checkers 'javascript-jshint 'typescript-tslint)
  (if (and buffer-file-name
           (or (f-ext-p buffer-file-name "js")
               (f-ext-p buffer-file-name "jsx")))
      (zc-flycheck/disable-checkers 'lsp) ; JS are shit!
    (zc-flycheck/disable-checkers 'javascript-eslint)))

;;;###autoload
(defun zc-typescript/disable-flycheck-for-flow ()
  (when (and buffer-file-name
             (string= (f-ext buffer-file-name) "js")
             (save-excursion (goto-char (point-min))
                             (search-forward "@flow" nil t)))
    (zc-flycheck/disable-checkers 'typescript-tide)))

;;;###autoload
(defun zc-typescript/disable-flycheck-for-node-modules ()
  (when (and buffer-file-name
             (s-contains-p "/node_modules/" buffer-file-name))
    (apply 'zc-flycheck/disable-checkers
           (->> flycheck-checkers
                (-map #'symbol-name)
                (--filter (or (string-prefix-p "javascript" it)
                              (string-prefix-p "typescript" it)))
                (-map #'intern)))))

;;;###autoload
(defun zc-typescript/tide-cleanup-processes-h ()
  "Clean up dangling tsserver processes if there are no more buffers with
`tide-mode' active that belong to that server's project."
  (when tide-mode
    (unless (cl-loop with project-name = (tide-project-name)
                     for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'tide-mode buf)
                             (with-current-buffer buf
                               (string= (tide-project-name) project-name)))
                     return buf)
      (kill-process (tide-current-server)))))


;; Tide

;;;###autoload
(defun zc-typescript/tide-stop-all-servers ()
  "Kill all tide process sentinels and cleanup projects."
  (interactive)
  (maphash
   (lambda (project process)
     (tide-net-sentinel process "done"))
   tide-servers))

;;;###autoload
(defun zc-typescript/linter-fix-file ()
  (interactive)
  (shell-command (concat "tslint --fix " (buffer-file-name)))
  (revert-buffer t t))

;;;###autoload
(defun zc-typescript/tide-eldoc-maybe-show (orig-fn text &optional cb)
  "Advice `tide-eldoc-maybe-show' with magics.

Documentations with multiple lines will expand the echo area,
which is very distracted.

Use `tide-documentation-at-point' to show complex types, need
to enable `tide-always-show-documentation'."
  (-let (((head . tail) (s-lines text)))
    (setq text (cond ((< (length head) 200)
                      (->> text
                           (s-replace-regexp (rx (+ (or space "\n"))) " ")
                           (s-truncate 200)))
                     ((null tail)
                      (concat (s-truncate 200 head) " ..."))
                     (t
                      (s-truncate 200 head))))
    (funcall orig-fn text cb)))
