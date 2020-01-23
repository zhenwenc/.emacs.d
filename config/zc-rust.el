(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'straight)
(require 'zc-paths)
(require 'zc-rust-funcs)

(autoload 'lsp-session "lsp")
(autoload 'lsp-workspace-folders-add "lsp")

(defconst zc-rust-analyzer-executable
  (f-join paths-vendor-dir "rust/ra_lsp_server-mac"))



(use-package rustic
  :straight t
  :defer t

  :hydra
  ((:mode rustic-mode :after lsp-mode)
   ("Eval"
    (("er" rustic-cargo-run   "run")
     ("eB" rustic-popup       "popup")
     ("eb" rustic-cargo-build "build")
     ("ec" rustic-cargo-check "check"))

    "Test"
    (("tt" zc-rust/cargo-test-dwim        "test")
     ("ta" zc-rust/cargo-test-all         "test all")
     ("tB" zc-rust/cargo-toggle-backtrace "backtrace"
      :toggle (zc-rust/cargo-backtrace-enabled-p)))))

  :preface
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

  :preface
  (defun zc-rust/download-rls-packages (&optional forced)
    "Download Rust Language Server required components with
rustup if not already installed."
    (interactive "P")
    (unless (and (not forced)
                 (executable-find "rls"))
      (zc-rust/download-rust-packages)
      ;; Install the required rust components with rustup.
      (shell-command "rustup component add rls")
      (message "Downloaded Rust Language Server!")))

  :preface
  (defun zc-rust/download-rust-analyzer (&optional forced)
    "Download rust-analyzer Language Server binary `ra_lsp_server'
if not already installed."
    (interactive "P")
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
                    (seq-find (-lambda (v) (s-equals? "ra_lsp_server-mac"
                                                      (alist-get 'name v)))
                              it)
                    (alist-get 'browser_download_url it))))
      (chmod zc-rust-analyzer-executable 493)
      ;; Rust Analyzer needs sources of rust standard library
      (zc-rust/download-rust-packages)
      (message "Downloaded Rust Language Server!")))

  :preface
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

  :config/el-patch
  (defun rustic-buffer-workspace (&optional nodefault)
    "Get the workspace root.
If NODEFAULT is t, return nil instead of `default-directory' if directory is
not in a rust project."
    (el-patch-let
        (($old (locate-dominating-file
                (or buffer-file-name default-directory) "Cargo.toml"))
         ;; HACK: Use package instead of crate root for workspace
         ($new (->> (concat "cargo metadata --format-version=1 --offline "
                            "| jq -rM '.workspace_root'")
                    (shell-command-to-string)
                    (s-trim-right)
                    (f-slash))))
      (let ((dir (el-patch-swap $old $new)))
        (if dir (expand-file-name dir)
          (if nodefault nil default-directory)))))

  :config/el-patch
  (defun rustic-format-file-sentinel (proc output)
    "Sentinel for rustfmt processes when formatting a file."
    (el-patch-let
        (($old (revert-buffer t t))
         ($new (progn
                 ;; HACK: Auto delete rustfmt buffer if succeed
                 (-when-let (win (get-buffer-window proc-buffer))
                   (delete-window win))
                 (revert-buffer t t))))
      (let ((proc-buffer (process-buffer proc)))
        (with-current-buffer proc-buffer
          (if (string-match-p "^finished" output)
              (progn
                (with-current-buffer next-error-last-buffer
                  (el-patch-swap $old $new)))
            (goto-char (point-min))
            (funcall rustic-format-display-method proc-buffer)
            (message "Rustfmt error."))))))

  :hook (rustic-mode . zc-rust/setup)

  :custom-face
  (rustic-errno-face                  ((t (:foreground ,(doom-color 'error)))))
  (rustic-cargo-outdated-upgrade-face ((t (:foreground ,(doom-color 'success)))))

  :init
  ;; The auto-LSP setup doesn't compatible.
  (setq rustic-lsp-setup-p nil)

  ;; HACK: Fix find current function issue.
  (advice-add 'rustic-cargo--get-current-fn-name :around #'zc-rust/get-current-fn-name)

  :config
  ;; Disable some unused features
  (setq rustic-display-spinner nil
        rustic-flycheck-setup-mode-line-p nil)

  ;; Display rustfmt errors without popping to the buffer.
  (setq rustic-format-display-method 'display-buffer
        rustic-format-on-save t
        rustic-lsp-format t)

  ;; The default ansi colors looks better in terminal.
  (setq rustic-ansi-faces (if (display-graphic-p)
                              (vector "black"
                                      (doom-color 'red)
                                      (doom-color 'green)
                                      (doom-color 'yellow)
                                      (doom-color 'blue)
                                      (doom-color 'magenta)
                                      (doom-color 'cyan)
                                      "white")
                            rustic-ansi-faces))

  ;; ;; Customize LSP backend
  (with-eval-after-load 'lsp
    (setq rustic-lsp-server 'rust-analyzer)
    (setq lsp-rust-server   'rust-analyzer)
    (setq lsp-rust-analyzer-use-client-watching nil)
    (setq lsp-rust-analyzer-server-command `(,zc-rust-analyzer-executable)))

  ;; Customize RLS server
  (with-eval-after-load 'lsp-rust
    ;; Only index the project when a file is saved
    (setq lsp-rust-build-on-save t))

  ;; Enhance smartparens
  (with-eval-after-load 'smartparens
    (require 'smartparens-rust)
    (sp-with-modes '(rustic-mode)
      ;; We have to port the configs to rustic from rust-mode.
      ;; https://github.com/Fuco1/smartparens/blob/master/smartparens-rust.el
      (sp-local-pair "'" "'"
                     :unless '(sp-in-comment-p
                               sp-in-string-quotes-p
                               sp-in-rust-lifetime-context)
                     :post-handlers '(:rem sp-escape-quotes-after-insert))
      (sp-local-pair "<" ">"
                     :when '(sp-rust-filter-angle-brackets)
                     :skip-match 'sp-rust-skip-match-angle-bracket)

      ;; Eagerly expand || to closure form with yasnippet.
      (sp-local-pair "|" "|"
                     :unless '(sp-in-comment-p sp-in-string-quotes-p)
                     :post-handlers '(("[d1]|" "SPC") ; Bitwise OR / Pattern alternative
                                      ("[d1] |" "=")  ; Bitwise OR & Assignment
                                      zc-rust/sp-expand-closure))))

  ;; Alias source code block language
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("rust" . rustic))))



(provide 'zc-rust)
