(eval-when-compile
  (require 'use-package))

(require 'general)



(use-package yasnippet
  :straight t

  :config
  (add-to-list 'yas-snippet-dirs
               (concat user-emacs-directory "snippets"))

  ;; Remove GUI dropdown prompt (prefer ivy)
  (delq #'yas-dropdown-prompt yas-prompt-functions)

  (yas-global-mode +1)

  ;; NOTE: yas-maybe-expand is a variable.
  (general-define-key :states 'insert :keymaps 'yas-minor-mode-map
                      "TAB" yas-maybe-expand))

(use-package zc-yas-funcs
  :after yasnippet)



(provide 'zc-yasnippet)
