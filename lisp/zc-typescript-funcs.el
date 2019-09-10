(require 's)
(require 'dash)
(require 'subr-x)

(autoload 'sp--looking-at-p "smartparens")
(autoload 'sp--looking-back-p "smartparens")
(autoload 'sp-end-of-sexp "smartparens")
(autoload 'sp-backward-whitespace "smartparens")
(autoload 'tide-net-sentinel "tide")

(defvar tide-servers)
(defvar lsp--symbol-kind)


;; Smartparens

(defun zc-typescript/sp-javadoc-expand (&rest _ignored)
  "Expand Javadoc style multiline comment block."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (insert "*"))
  (insert " * ")
  (save-excursion
    (insert "\n")
    (indent-according-to-mode))
  (indent-according-to-mode))

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

(defun zc-typescript/tide-load-tsconfig (path &rest _ignored)
  "Overrides `tide-load-tsconfig'."
  (when (not (file-exists-p path))
    (error "tsconfig file not found at %S." path))
  (condition-case nil
      (let ((json-object-type 'plist)
            (json (shell-command-to-string "tsc --showConfig")))
        (json-read-from-string json))
    (error '())))


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
