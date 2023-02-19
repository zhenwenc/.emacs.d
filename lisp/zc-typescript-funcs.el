(require 's)
(require 'dash)
(require 'subr-x)

(autoload 'sp--looking-at-p "smartparens")
(autoload 'sp--looking-back-p "smartparens")
(autoload 'sp-end-of-sexp "smartparens")
(autoload 'sp-backward-whitespace "smartparens")
(autoload 'tide-net-sentinel "tide")

(defvar tide-servers)
(defvar tide-project-configs)
(defvar lsp--symbol-kind)
(defvar flycheck-checkers)


;; Startup

(defun zc-typescript/set-node-modules-readonly ()
  (when (and (buffer-file-name)
             (s-contains-p "/node_modules/" buffer-file-name))
    (read-only-mode +1)))

(defun zc-typescript/add-node-modules-bin-to-path ()
  "Use binaries from node_modules, where available."
  (when-let (root (projectile-project-p))
    (make-local-variable 'exec-path)
    (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

(defun zc-typescript/disable-flycheck-linters ()
  "Linters are pretty slow, and we use Prettier anyway."
  (zc-flycheck/disable-checkers 'javascript-jshint 'typescript-tslint)
  (if (and buffer-file-name
           (or (f-ext-p buffer-file-name "js")
               (f-ext-p buffer-file-name "jsx")))
      (zc-flycheck/disable-checkers 'lsp) ; JS are shit!
    (zc-flycheck/disable-checkers 'javascript-eslint)))

(defun zc-typescript/disable-flycheck-for-flow ()
  (when (and buffer-file-name
             (string= (f-ext buffer-file-name) "js")
             (save-excursion (goto-char (point-min))
                             (search-forward "@flow" nil t)))
    (zc-flycheck/disable-checkers 'typescript-tide)))

(defun zc-typescript/disable-flycheck-for-node-modules ()
  (when (and buffer-file-name
             (s-contains-p "/node_modules/" buffer-file-name))
    (apply 'zc-flycheck/disable-checkers
           (->> flycheck-checkers
                (-map #'symbol-name)
                (--filter (or (string-prefix-p "javascript" it)
                              (string-prefix-p "typescript" it)))
                (-map #'intern)))))

(defun zc-typescript/setup-lsp-workspace ()
  "Handler for `lsp-after-open-hook' to setup the workspace.

TODO: Maybe set `lsp-enable-completion-at-point' to `nil'?
"
  (-when-let (workspace
              (->> (lsp-session)
                   (lsp--session-workspaces)
                   (--first (and (eq 'initialized (lsp--workspace-status it))
                                 (let* ((client (lsp--workspace-client it))
                                        (server (lsp--client-server-id client)))
                                   (eq server 'ts-ls))))))
    ;; Trigger completion by "<" or ")" freeze Emacs
    (-some--> (lsp--capability :completionProvider)
      (lsp-put it :triggerCharacters ["."]))
    ;; Disable annoying useless signature help
    (-some--> (lsp--capability :signatureHelpProvider)
      (lsp-put it :triggerCharacters []))))


;; Smartparens

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

(defun zc-typescript/sp-jsx-get-tag-name (&rest _ignored)
  "Return the JSX tag name inclosed in <> pair."
  (let* ((matched (save-excursion (sp-end-of-sexp)))
         (beg (plist-get matched :beg))
         (end (plist-get matched :end))
         (str (buffer-substring beg end))
         (sub (replace-regexp-in-string "/\\|<\\|>" "" str)))
    (string-trim (car (split-string sub " ")))))


;; Tide

(defun zc-typescript/tide-stop-all-servers ()
  "Kill all tide process sentinels and cleanup projects."
  (interactive)
  (maphash
   (lambda (project process)
     (tide-net-sentinel process "done"))
   tide-servers))

(defun zc-typescript/linter-fix-file ()
  (interactive)
  (shell-command (concat "tslint --fix " (buffer-file-name)))
  (revert-buffer t t))

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

(defun zc-typescript/tide-show-project-info (orig-fn version config-file-name)
  "Advice `tide-eldoc-maybe-show' with extra information."
  (funcall orig-fn version config-file-name)
  (let ((inhibit-read-only t)
        (config (gethash (tide-project-name) tide-project-configs)))
    (with-current-buffer "*tide-project-info*"
      (insert "\n\n")
      (let ((pos (point-max)))
        (json-insert config)
        (json-pretty-print pos (point-max))))))


;; LSP

;; TODO: Remove `zc-yas/test-buffer-p'
(defun zc-typescript/test-buffer-p ()
  "Return t if buffer name contains .(test|spec). infix."
  (string-match-p ".\\(test\\|spec\\)." (buffer-name)))

(defun zc-typescript/lsp-jest-re (name)
  "Return regexp matching a callback function signature."
  (eval `(rx ,name "(" (group-n 1 (+ anything)) ") callback")))

;;;###autoload
(defun zc-typescript/lsp-symbol-filter (sym)
  "Used by `zc-lsp/imenu-symbol-filter' for TypeScript.

This function assumes the LSP complection requests are *not*
cached, where it may mutate the elements."
  (-when-let* (((&hash "name") sym)
               (kind (lsp--get-symbol-type sym)))
    (cond
     ;; For test files, assuming Jest framework
     ((zc-typescript/test-buffer-p)
      (pcase name
        ;; For Jest function: "describe"
        ((and (pred (string-match (zc-typescript/lsp-jest-re "describe")))
              (app (match-string 1) desc))
         (--> (or (gethash "children" sym) [])
              (seq-filter (lambda (node)
                            (string= "Function" (lsp--get-symbol-type node)))
                          it)
              (puthash "children" (vconcat it) sym))
         (puthash "name" (or desc name) sym))
        ;; For Jest function: "it"
        ((and (pred (string-match (zc-typescript/lsp-jest-re "it")))
              (app (match-string 1) desc))
         (remhash "children" sym)
         (puthash "name" (or desc name) sym))))
     ;; Remove private scope variable nodes
     ((member kind '("Method" "Function"))
      (remhash "children" sym))))
  t)

;;;###autoload
(defun zc-typescript/lsp-filter-symbols (symbols)
  "Used by `zc-lsp/imenu-symbols-filter' for TypeScript."
  (let ((-compare-fn (-lambda ((&hash "kind" k1 "name" n1)
                               (&hash "kind" k2 "name" n2))
                       (and (equal n1 n2) (equal k1 k2)))))
    (-uniq symbols)))



(provide 'zc-typescript-funcs)
