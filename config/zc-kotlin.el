(eval-when-compile
  (require 'use-package))



(use-package kotlin-mode
  :straight t
  :defer t
  :config
  (setq kotlin-tab-width 2))

;; Major mode for Groovy and Grails files.
(use-package groovy-mode
  :straight t
  :mode "\\.gradle\\'")



(provide 'zc-kotlin)
