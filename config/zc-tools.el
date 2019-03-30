(eval-when-compile
  (require 'use-package))

(use-package graphql-mode
  :straight t
  :defer t
  :mode "graphql\\'")

(use-package docker
  :straight t
  :defer t
  :commands (docker))

(use-package dockerfile-mode
  :straight t
  :defer t
  :mode "Dockerfile\\'")

(use-package terraform-mode
  :straight t
  :defer t
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  ;; Ensure the same indent level as 'terraform fmt'
  (setq terraform-indent-level 2))

(provide 'zc-tools)
