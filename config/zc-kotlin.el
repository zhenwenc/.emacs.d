(eval-when-compile
  (require 'use-package))



(use-package kotlin-mode
  :straight t
  :defer t)

;; Major mode for Groovy and Grails files.
(use-package groovy-mode
  :straight t
  :defer t
  :mode "\\.gradle\\'")

;; Provides support for running Android SDK subprocesses like the
;; emulator, logcat, ddms and ant.
(use-package android-mode
  :straight t
  :defer t
  :mode "build\\.gradle\\'")



(provide 'zc-kotlin)
