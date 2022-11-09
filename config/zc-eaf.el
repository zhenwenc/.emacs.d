(eval-when-compile
  (require 'el-patch)
  (require 'use-package))

(use-package eaf
  :straight (eaf :type git
                 :host github
                 :repo "emacs-eaf/emacs-application-framework"
                 :files ("*.el" "*.py" "core" "app" "*.json")
                 ;; straight won't search for these packages when we make further use-package invocations for them
                 :includes (eaf-pdf-viewer eaf-browser)
                 :pre-build (("which" "python")
                             ("python" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps")))
  ;; Evil mode doesn't work well with EAF keybindings.
  :init (evil-set-initial-state 'eaf-mode 'emacs))

(use-package eaf-browser
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t))

(use-package eaf-pdf-viewer)



(provide 'zc-eaf)
