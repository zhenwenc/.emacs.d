(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'straight)
(require 'zc-rust-funcs)

(autoload 'lsp-workspace-folders-add "lsp")



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
    "Download Rust Language Server required packages."
    (zc-rust/download-rls-packages)
    (lsp-workspace-folders-add (rustic-buffer-workspace))
    (lsp-deferred))

  :preface
  (defun zc-rust/download-rls-packages ()
    "Download Rust Language Server required components with
rustup if not already installed."
    (unless (executable-find "rustup")
      (error "No rustup executable found!"))
    ;; Install the required rust components with rustup.
    (let ((pkgs '("rls" "clippy" "rustfmt" "rust-analysis" "rust-src"))
          (-compare-fn #'s-starts-with-p))
      (dolist (package
               (--> (shell-command-to-string
                     "rustup component list --installed")
                    (s-split "\n" it t)
                    (-remove (-partial #'-contains? it) pkgs)))
        (shell-command (concat "rustup component add " package))
        (message "Installed %s!" package))))

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
  (advice-add 'rustic-setup-rls :override #'ignore)

  ;; HACK: Fix find current function issue.
  (advice-add 'rustic-cargo--get-current-fn-name :around #'zc-rust/get-current-fn-name)

  :config
  ;; Disable some unused features
  (setq rustic-display-spinner nil
        rustic-flycheck-setup-mode-line-p nil)

  ;; Display rustfmt errors without popping to the buffer.
  (setq rustic-format-display-method 'display-buffer)

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
