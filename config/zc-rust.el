(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'straight)



(use-package rustic
  :straight t
  :defer t

  :hydra
  ((:mode rustic-mode :after lsp-mode)
   ("Build"
    (("bB" rustic-popup       "popup")
     ("bb" rustic-cargo-build "build")
     ("bc" rustic-cargo-check "check"))

    "Execute & Test"
    (("er" rustic-cargo-run   "run")
     ("et" rustic-cargo-test  "test"))))

  :preface
  (defun zc-rust/setup ()
    "Download Rust Language Server required packages."
    (unless (executable-find "rustup")
      (error "No rustup executable found!"))
    ;; Install the required rust components with rustup.
    (let ((pkgs '("rustfmt" "rls" "rust-analysis" "rust-src" "clippy"))
          (-compare-fn #'s-starts-with-p))
      (dolist (package (--> (shell-command-to-string
                             "rustup component list --installed")
                            (s-split "\n" it t)
                            (-remove (-partial #'-contains? it) pkgs)))
        (shell-command (concat "rustup component add " package))
        (message "Installed %s!" package)))
    (lsp-deferred))

  :hook (rustic-mode . zc-rust/setup)

  :custom-face
  (rustic-errno-face                  ((t (:foreground ,(doom-color 'error)))))
  (rustic-cargo-outdated-upgrade-face ((t (:foreground ,(doom-color 'success)))))

  :init
  ;; Disable some unused features
  (setq rustic-display-spinner nil
        rustic-flycheck-setup-mode-line-p nil)

  ;; The auto-LSP setup doesn't compatible.
  (advice-add 'rustic-setup-rls :override #'ignore)

  :config
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

  (with-eval-after-load 'org
    ;; Alias source code block language
    (add-to-list 'org-src-lang-modes '("rust" . rustic))))



(provide 'zc-rust)
