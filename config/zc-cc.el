(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'zc-hydra-funcs)



;; https://www.emacswiki.org/emacs/ObjectiveCMode
(use-package objc-mode
  :defer t
  :config

  ;; Fix syntax highlighting for .h files defines an @interface is objc-mode instead of c-mode.
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (re-search-forward "@\\<interface\\>"
                                            magic-mode-regexp-match-limit t)))
                 . objc-mode))
  )



(provide 'zc-cc)
