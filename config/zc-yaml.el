(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-mode . company-mode)
         (yaml-mode . flycheck-mode-on-safe)))

(provide 'zc-yaml)
