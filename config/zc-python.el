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
  :commands zc-python/lsp-python-ms-setup
  :hook ((python-mode . zc-python/lsp-python-ms-setup)
         (python-mode . lsp-deferred))
  :config
  (setq lsp-python-ms-dir (concat paths-vendor-dir "mspyls/")
        lsp-python-ms-executable (concat lsp-python-ms-dir
                                         "Microsoft.Python.LanguageServer"))

  (defun zc-python/lsp-python-ms-setup (&optional forced)
    "Downloading Microsoft Python Language Server to path specified.
With prefix, FORCED to redownload the server.

Ref: Centaur Emacs"
    (interactive "P")
    (unless (and (not forced)
                 (file-exists-p lsp-python-ms-dir))
      (let ((temp-file (make-temp-file "mspyls" nil ".zip"))
            (unzip (if (executable-find "unzip")
                       "bash -c 'mkdir -p %2$s && unzip -qq %1$s -d %2$s'"
                     (user-error "Required 'unzip' executable.")))
            (url (format (concat "https://pvsc.azureedge.net/python-language-server-stable"
                                 "/Python-Language-Server-%s-x64.0.2.96.nupkg")
                         (pcase system-type
                           ('windows-nt "win")
                           ('darwin     "osx")
                           ('gnu/linux  "linux")
                           (_ (user-error "Unknown system type!"))))))
        (url-copy-file url temp-file 'overwrite)
        (if (file-exists-p lsp-python-ms-dir)
            (delete-directory lsp-python-ms-dir 'recursive))
        (shell-command (format unzip temp-file lsp-python-ms-dir))
        (if (file-exists-p lsp-python-ms-executable)
            (chmod lsp-python-ms-executable #o755))
        (message "Downloaded Microsoft Python Language Server!")))))



(provide 'zc-python)
