(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'zc-yas-funcs)



(use-package yasnippet
  :straight t

  :general
  (:keymaps 'yas-minor-mode-map :states 'insert
   [tab] yas-maybe-expand)

  :init
  ;; The yas/ functions are obsoleted since v0.8
  (setq yas-alias-to-yas/prefix-p nil)
  (setq yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))

  :config
  ;; Remove GUI dropdown prompt (prefer ivy)
  (delq #'yas-dropdown-prompt yas-prompt-functions)
  (yas-global-mode +1))



(provide 'zc-yasnippet)
