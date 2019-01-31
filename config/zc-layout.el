(eval-when-compile
  (require 'use-package))

(require 'zc-layout-funcs)



(use-package eyebrowse
  :straight t
  :config
  (progn
    (setq eyebrowse-tagged-slot-format "%s [%t]")

    ;; Unset all default key bindings
    (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)

    (eyebrowse-mode t)))

(use-package winum
  :straight t
  :general
  ("M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4
   "M-5" #'winum-select-window-5)
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-ignored-buffers '(" *which-key*"))
    (winum-mode)))



(provide 'zc-layout)
