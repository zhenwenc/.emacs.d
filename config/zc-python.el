(eval-when-compile
  (require 'use-package))

(require 'zc-hydra-funcs)



;; Requirements:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :straight t
  :defer t
  :defines (gud-pdb-command-name pdb-path)
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda () (process-query-on-exit-flag (get-process "Python")))))

;; Live Coding in Python
(use-package live-py-mode
  :disabled t
  :straight t
  :defer t
  :hook (python-mode . live-py-mode))

;; Format using YAPF, required:
;;   pip install yapf
(use-package yapfify
  :disabled t
  :straight t
  :hook (python-mode . yapf-mode))



(use-package lsp-python-ms
  :disabled t
  :straight t
  :hook (python-mode . zc-python/init-python-ms)
  :init
  (defun zc-python/init-python-ms ()
    (require 'lsp)
    ;; Connect to the current LSP workspace session if available.
    (-when-let (workspace
                (->> (lsp-session)
                     (lsp--session-workspaces)
                     (--filter (and (eq 'initialized (lsp--workspace-status it))
                                    (eq 'mspyls (lsp--client-server-id (lsp--workspace-client it)))))
                     (--first (f-ancestor-of? (lsp--workspace-root it)
                                              (buffer-file-name)))))
      (lsp-deferred)))
  (setq lsp-python-ms-dir (concat paths-vendor-dir "mspyls/")))

(use-package lsp-pyright
  :straight t
  :defines lsp-pyright-python-executable-cmd
  :hook (python-mode . zc-python/init-pyright)
  :init
  (defun zc-python/init-pyright ()
    (require 'lsp)
    (require 'lsp-pyright)
    ;; Connect to the current LSP workspace session if available.
    (-when-let (workspace
                (->> (lsp-session)
                     (lsp--session-workspaces)
                     (--filter (and (eq 'initialized (lsp--workspace-status it))
                                    (eq 'pyright (lsp--client-server-id (lsp--workspace-client it)))))
                     (--first (f-ancestor-of? (lsp--workspace-root it)
                                              (buffer-file-name)))))
      (lsp-deferred)))

  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))



(provide 'zc-python)
