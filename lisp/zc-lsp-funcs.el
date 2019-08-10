(require 's)
(require 'ht)
(require 'dash)

(autoload 'company-grab "company")
(autoload 'company-grab-symbol-cons "company")

(defvar lsp-ui-doc-mode)
(defvar lsp-ui-sideline-mode)


;; IMenu

(defun zc-lsp/imenu-symbol-filter (orig-fn sym)
  "Advice `lsp--symbol-filter' with language specific filters."
  (or (funcall orig-fn sym)
      (when (derived-mode-p 'typescript-mode)
        (not (zc-typescript/lsp-symbol-filter sym)))))

(defun zc-lsp/imenu-filter-symbols (orig-fn symbols)
  "Advice `lsp--imenu-filter-symbols' with language specific filters."
  (--> (funcall orig-fn symbols)
       (cond
        ((derived-mode-p 'typescript-mode)
         (zc-typescript/lsp-filter-symbols it))
        (t it))))


;; Auto completion

(defun zc-lsp/in-string-p ()
  (nth 3 (syntax-ppss)))

(defun zc-lsp/in-comment-p ()
  (nth 4 (syntax-ppss)))

;; Original from `tide-completion-prefix'.
(defun zc-lsp/ts-completion-prefix ()
  (if (and (zc-lsp/in-string-p)
           (looking-back
            (rx (or (and "import" (1+ space) (or ?\" ?') (0+ (not (any ?\" ?'))))
                    (and "from" (1+ space) (or ?\" ?') (0+ (not (any ?\" ?'))))
                    (and "import(" (or ?\" ?') (0+ (not (any ?\" ?'))))
                    (and "require(" (or ?\" ?') (0+ (not (any ?\" ?'))))))
            1))
      (cons (company-grab (rx (or ?/ ?\" ?') (group (0+ (not (any ?\" ?'))))) 1) t)
    (company-grab-symbol-cons "\\." 1)))

;;;###autoload
(defun zc-lsp/company-lsp (orig-fn command &rest args)
  "Advice `company-lsp' with language specific features.

- TypeScript: trigger completion for imports.
"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-lsp))
    (prefix (cond ((not (bound-and-true-p lsp-mode)) 'stop)
                  ((derived-mode-p 'typescript-mode)
                   (or (zc-lsp/ts-completion-prefix) 'stop))
                  (t (apply orig-fn (cons command args)))))
    (t (apply orig-fn (cons command args)))))


;; Execute code lens commands

(defun zc-lsp/lens-command-candidates ()
  "Returns the available lenses with command in the buffer
for completion."
  (require 'ht)
  (->> (lsp-request "textDocument/codeLens"
                    `(:textDocument
                      (:uri ,(lsp--path-to-uri buffer-file-name))))
       (--filter (gethash "command" it))
       (--map (-let* (((command &as &hash "command" (&hash "title" "arguments")) it)
                      ((&hash "env" "binary" "args" "cwd") (seq-first arguments))
                      (cwd (or cwd (lsp-workspace-root) default-directory))
                      (cmd-envs (s-join " " (ht-amap (format "%s=%s" key value) env)))
                      (cmd-args (s-join " " args))
                      (cmd (format "%s %s %s" cmd-envs binary cmd-args)))
                ;; TODO Handle non-command lenses? see `lsp-lens-show'.
                (pcase command
                  (rls.run (cons (format "%s > %s %s" title binary cmd-args)
                                 `(:cwd ,cwd :cmd ,cmd))))))))

(defun zc-lsp/lens-command-action (x)
  "Execute the selected lens candidate LENS."
  (-let* (((&plist :cwd cwd :cmd cmd) (cdr x))
          (default-directory cwd))
    (compile cmd)))

;;;###autoload
(defun zc-lsp/lens-command-run ()
  "Prompt with available lens commands and execute."
  (interactive)
  (ivy-read "Command: " (zc-lsp/lens-command-candidates)
            :action  'zc-lsp/lens-command-action
            :history 'zc-lsp/lens-command-run
            :caller  'zc-lsp/lens-command-run
            :require-match t))


;; Misc.

(defun zc-lsp/infer-project-root (orig-fn &rest args)
  "Advice `lsp--suggest-project-root' to be smarter."
  (or (and (eq major-mode 'rustic-mode) ;; Rust
           (rustic-buffer-workspace t))
      (apply orig-fn args)))


;; Commands

;;;###autoload
(defun zc-lsp/workspace-maybe-restart ()
  "Enable `lsp-mode' if not currently enabled, otherwise
delegate to `lsp-workspace-restart'."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (call-interactively 'lsp-workspace-restart)
    (call-interactively 'lsp)))

;;;###autoload
(defun zc-lsp/switch-to-std-log-buffer ()
  "Switch to standard LSP *lsp-log* buffer."
  (interactive)
  (let ((buffer (get-buffer "*lsp-log*")))
    (unless buffer
      (error "No LSP log buffer found."))
    (pop-to-buffer buffer)))

;;;###autoload
(defun zc-lsp/toggle-lsp-ui-doc-mode ()
  (interactive)
  (if lsp-ui-doc-mode
      (progn
        (lsp-ui-doc-mode -1)
        (lsp-ui-doc-hide)
        (message "Disabled LSP Document!"))
    (lsp-ui-doc-mode +1)
    (message "Enabled LSP Document!")))

;;;###autoload
(defun zc-lsp/execute-code-action-dwim ()
  (interactive)
  (cond ((lsp-code-actions-at-point)
         (call-interactively #'lsp-execute-code-action))
        (lsp-ui-sideline-mode
         (lsp-ui-sideline-apply-code-actions))
        (t (user-error "No code action available"))))



(provide 'zc-lsp-funcs)
