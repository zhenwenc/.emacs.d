(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")

  :preface
  (defun zc-yaml/override-font-lock-faces ()
    (face-remap-add-relative 'font-lock-variable-name-face
                             :foreground (doom-color 'violet)))

  :hook ((yaml-mode . company-mode)
         (yaml-mode . flycheck-mode-on-safe)
         (yaml-mode . zc-yaml/override-font-lock-faces)))

(provide 'zc-yaml)
