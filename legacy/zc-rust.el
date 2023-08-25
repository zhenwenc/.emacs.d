(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'straight)
(require 'zc-paths)
(require 'zc-rust-funcs)

(autoload 'lsp-session "lsp")
(autoload 'lsp-workspace-folders-add "lsp")



(use-package rustic
  :straight t
  :defer t

  :hydra
  ((:mode rustic-mode :after lsp-mode)
   ("Eval"
    (("er" rustic-cargo-run    "run")
     ("eB" rustic-popup        "popup")
     ("eb" rustic-cargo-build  "build")
     ("eC" rustic-cargo-check  "check")
     ("ec" rustic-cargo-clippy "clippy"))

    "Test"
    (("tt" zc-rust/cargo-test-dwim        "test")
     ("ta" zc-rust/cargo-test-all         "test all")
     ("tB" zc-rust/cargo-toggle-backtrace "backtrace"
      :toggle (zc-rust/cargo-backtrace-enabled-p)))

    "Docs"
    (("hI" lsp-rust-analyzer-inlay-hints-mode "inlay" :toggle t)
     ("hm" lsp-rust-analyzer-expand-macro     "expand macro"))))

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
      (ignore-errors
        (let ((proc-buffer (process-buffer proc)))
          (with-current-buffer proc-buffer
            (if (string-match-p "^finished" output)
                (progn
                  (with-current-buffer next-error-last-buffer
                    (el-patch-swap $old $new)))
              (goto-char (point-min))
              (funcall rustic-format-display-method proc-buffer)
              (message "Rustfmt error.")))))))

  :hook (rustic-mode . zc-rust/setup)

  :custom-face
  (rustic-errno-face                  ((t (:foreground ,(doom-color 'error)))))
  (rustic-cargo-outdated-upgrade-face ((t (:foreground ,(doom-color 'success)))))

  :init
  ;; The auto-LSP setup doesn't compatible.
  (setq rustic-lsp-setup-p nil)

  ;; HACK: Variable will be used when loading `lsp-mode'.
  (defvar lsp-rust-server 'rust-analyzer)

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

  ;; Customize LSP backend
  (with-eval-after-load 'lsp-mode
    (setq rustic-lsp-server 'rust-analyzer)
    (setq lsp-rust-analyzer-use-client-watching nil)
    (setq lsp-rust-analyzer-cargo-watch-command "clippy")
    (setq lsp-rust-analyzer-cargo-watch-args ["--tests"])
    (setq lsp-rust-analyzer-server-command `(,zc-rust-analyzer-executable)))

  ;; Customize RLS server
  (with-eval-after-load 'lsp-rust
    ;; Only index the project when a file is saved
    (setq lsp-rust-build-on-save t)))



(provide 'zc-rust)
