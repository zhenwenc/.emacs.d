(eval-when-compile
  (require 'use-package))

(require 'zc-hydra-funcs)



;; Requirements:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :straight t
  :defines (gud-pdb-command-name pdb-path)
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda () (process-query-on-exit-flag (get-process "Python")))))

;; Live Coding in Python
(use-package live-py-mode
  :straight t
  :after python)

;; Format using YAPF, required:
;;   pip install yapf
(use-package yapfify
  :straight t
  :hook (python-mode . yapf-mode))



;; Microsoft python-language-server support
(use-package lsp-python-ms
  :straight t
  :preface
  (defun zc-python/init-python-ms ()
    (require 'lsp-python-ms)
    (lsp-deferred))
  :hook (python-mode . zc-python/init-python-ms)
  :init
  (setq lsp-python-ms-dir (concat paths-vendor-dir "mspyls/")))



(provide 'zc-python)
