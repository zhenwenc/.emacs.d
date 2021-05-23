(eval-when-compile
  (require 'use-package))



(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")

  :hook ((yaml-mode . company-mode)
         (yaml-mode . flycheck-mode-on-safe)
         (yaml-mode . zc-yaml/override-font-lock-faces))

  :config
  (require 'lsp)

  (defun zc-yaml/override-font-lock-faces ()
    (face-remap-add-relative 'font-lock-variable-name-face
                             :foreground (doom-color 'violet)))

  ;; Customize YAMLLS server
  (with-eval-after-load 'lsp-yaml
    (setq lsp-yaml-hover nil)))



(provide 'zc-yaml)
