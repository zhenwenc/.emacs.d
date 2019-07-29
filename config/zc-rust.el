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
     ("bb" rustic-cargo-build "build"))))

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
  (advice-add 'rustic-setup-rls :override #'ignore))



(provide 'zc-rust)
