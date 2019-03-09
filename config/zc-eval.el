(eval-when-compile
  (require 'use-package))



(use-package compile
  :defer t
  :after evil

  :general
  (:states 'motion :keymaps 'compilation-mode-map
           "h" #'evil-backward-char)

  :preface
  (defun zc-core/colorize-compilation-buffer ()
    (unless (derived-mode-p 'rg-mode)
      (with-silent-modifications
        (ansi-color-apply-on-region compilation-filter-start (point)))))

  :hook (compilation-filter . zc-core/colorize-compilation-buffer)

  :init
  (setq compilation-environment '("TERM=screen-256color")
        compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

(use-package quickrun
  :straight t
  :defer nil
  :config
  (setq quickrun-focus-p nil)

  (quickrun-add-command "typescript"
    '((:command . "ts-node")
      (:exec . ("%c -T %o %s %a"))
      (:compile-only . "%c %o %s %s")
      (:compile-conf . ((:compilation-mode . nil)
                        (:mode . typescript-mode)))
      (:description . "Run TypeScript script"))
    :mode 'typescript
    :override t))



(provide 'zc-eval)
