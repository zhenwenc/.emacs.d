(require 's)
(require 'dash)

(autoload 'yas-lookup-snippet "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")
(autoload 'sp--looking-back-p "smartparens")
(autoload 'rustic-compilation-mode "rustic")
(autoload 'rustic-cargo--get-current-fn-name "rustic")
(autoload 'rustic-cargo--get-current-fn-fullname "rustic")

(defvar compilation-last-buffer)
(defvar rustic-cargo-bin)
(defvar rustic-compile-backtrace)


;; Helper Functions

(defun zc-rust/get-current-fn-name (orig-fn)
  "Return fn name around point of nil.

Advice `rustic-cargo--get-current-fn-name' with fix for
supporting function with `mod' declared."
  (let ((rustic-top-item-beg-re
         (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*"
                 (regexp-opt '("fn")) "\\_>")))
    (funcall orig-fn)))

(defun zc-rust/cargo-backtrace-enabled-p ()
  "Return t if RUST_BACKTRACE set to any value other than '0'."
  (not (equal "0" rustic-compile-backtrace)))


;; Commands

(defun zc-rust/cargo-test-run (command)
  "Run the COMMAND with `compile'."
  (compile (s-join " " (-flatten command)))
  (with-current-buffer compilation-last-buffer
    (rustic-compilation-mode)))

;;;###autoload
(defun zc-rust/cargo-test-dwim (&optional arg)
  "Run 'cargo test' command.

If ARG is not nil, prompt for test command arguments,
otherwise only test the function near point or all test
cases if not found any test function."
  (interactive "P")
  (let* ((backtrace (format "RUST_BACKTRACE=%s" rustic-compile-backtrace))
         (command  `(,backtrace ,rustic-cargo-bin "test"))
         (args (if arg (read-from-minibuffer "Cargo test arguments: ")
                 (--when-let (rustic-cargo--get-current-fn-fullname)
                   (list "--nocapture" it)))))
    (zc-rust/cargo-test-run (list command "--" args))))

;;;###autoload
(defun zc-rust/cargo-test-all ()
  "Run 'cargo test' command."
  (interactive)
  (zc-rust/cargo-test-run (list rustic-cargo-bin "test")))

;;;###autoload
(defun zc-rust/cargo-toggle-backtrace (&optional arg)
  "Enable or disable RUST_BACKTRACE."
  (interactive "P")
  (setq rustic-compile-backtrace
        (if (zc-rust/cargo-backtrace-enabled-p)
            "0"
          (if arg "full" "1"))))


;; Smartparens

(defun zc-rust/sp-expand-closure (id action &rest _ignored)
  (--when-let (and (eq action 'skip-closing-pair)
                   (or (sp--looking-back-p "= ||") ; declare variable
                       (sp--looking-back-p ", ||") ; as argument
                       (sp--looking-back-p "(||")) ; as first argument
                   (yas-lookup-snippet "closure" 'rustic-mode))
    (backward-delete-char 2)
    (yas-expand-snippet it)))



(provide 'zc-rust-funcs)
