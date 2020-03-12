(require 'f)
(require 's)
(require 'dash)
(require 'zc-paths)

(autoload 'yas-lookup-snippet "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")
(autoload 'sp--looking-back-p "smartparens")
(autoload 'rustic-compilation-mode "rustic")
(autoload 'rustic-cargo--get-current-fn-name "rustic")
(autoload 'rustic-cargo--get-current-fn-fullname "rustic")

(defvar compilation-last-buffer)
(defvar rustic-cargo-bin)
(defvar rustic-compile-backtrace)
(defvar lsp-rust-server)

(defconst zc-rust-analyzer-executable
  (f-join paths-vendor-dir "rust/ra_lsp_server-mac"))


;; Startup

(defun zc-rust/setup ()
  (require 'lsp)
  ;; Prefer LSP checker
  (zc-flycheck/disable-checkers 'rustic-clippy)
  ;; Connect to the current LSP workspace session if available.
  (-when-let (workspace
              (->> (lsp-session)
                   (lsp--session-workspaces)
                   (--filter (and (eq 'initialized (lsp--workspace-status it))
                                  (let* ((client (lsp--workspace-client it))
                                         (server (lsp--client-server-id client)))
                                    (eq server lsp-rust-server))))
                   (--first (f-ancestor-of? (lsp--workspace-root it)
                                            (buffer-file-name)))))
    ;; Ensure RLS executable is available.
    (pcase lsp-rust-server
      ('rls           (zc-rust/download-rls-packages))
      ('rust-analyzer (zc-rust/download-rust-analyzer)))
    ;; (lsp-workspace-folders-add (rustic-buffer-workspace))
    (lsp-deferred)))

(defun zc-rust/download-rls-packages (&optional forced)
  "Download Rust Language Server required components with
rustup if not already installed."
  (interactive)
  (unless (and (not forced)
               (executable-find "rls"))
    (zc-rust/download-rust-packages)
    ;; Install the required rust components with rustup.
    (shell-command "rustup component add rls")
    (message "Downloaded Rust Language Server!")))

(defun zc-rust/download-rust-analyzer (&optional forced)
  "Download rust-analyzer Language Server binary `ra_lsp_server'
if not already installed."
  (interactive)
  (unless (and (not forced)
               (executable-find zc-rust-analyzer-executable))
    (message "Downloading rust-analyzer Rust Language Server...")
    (shell-command
     (format "curl -L --create-dirs -s -o %s %s" zc-rust-analyzer-executable
             (--> (format "https://api.github.com/repos/%1$s/%1$s/releases/%s"
                          "rust-analyzer" "latest")
                  (shell-command-to-string (concat "curl -s " it))
                  (json-read-from-string it)
                  (alist-get 'assets it) ;; assets array
                  (seq-find (-lambda (v) (s-equals? "rust-analyzer-mac"
                                                    (alist-get 'name v)))
                            it)
                  (alist-get 'browser_download_url it))))
    (chmod zc-rust-analyzer-executable 493)
    ;; Rust Analyzer needs sources of rust standard library
    (zc-rust/download-rust-packages)
    (message "Downloaded Rust Language Server!")))

(defun zc-rust/download-rust-packages ()
  "Install Rust Language Server required common packages."
  (unless (executable-find "rustup")
    (error "No rustup executable found!"))
  ;; Install required rust components with rustup.
  (let ((pkgs '("clippy" "rustfmt" "rust-analysis" "rust-src"))
        (-compare-fn #'s-starts-with-p))
    (dolist (package
             (--> (shell-command-to-string
                   "rustup component list --installed")
                  (s-split "\n" it t)
                  (-remove (-partial #'-contains? it) pkgs)))
      (shell-command (concat "rustup component add " package))
      (message "Installed %s!" package))))


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

(defun zc-rust/buffer-workspace-root (&optional nodefault)
  "Get the workspace root from Cargo metadata command.

If NODEFAULT is t, return nil instead of `default-directory' if directory is
not in a rust project."
  (-if-let* ((dir (->> (concat "cargo metadata --format-version=1 --offline "
                               "| jq -rM '.workspace_root'")
                       (shell-command-to-string)
                       (s-trim-right))))
      (when (f-exists? dir)
        (expand-file-name (f-slash dir)))
    (if nodefault nil default-directory)))


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
         (args (if arg (read-from-minibuffer
                        "Cargo test arguments: "
                        (file-name-sans-extension (f-filename buffer-file-name)))
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


;; Integration with `cargo-expand'

;;;###autoload
(defun zc-rust/cargo-expand ()
  "FIXME Run 'cargo expand' to expand macros."
  (interactive)
  (let* ((command (list rustic-cargo-bin "expand"))
         (args (read-from-minibuffer "Cargo expand: ")))
    (rustic-compilation-process-live)
    (rustic-compilation-start (concat (s-join " " command) " " args))))


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
