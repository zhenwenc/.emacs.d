(eval-when-compile
  (require 'use-package))

(require 's)
(require 'dash)



(defconst zc-eval/jest-error-rx
  (rx bol "at" (+? nonl)
      "("
      (group (+? nonl)) ":" ;; filename
      (group (+ digit)) ":" ;; line
      (group (+ digit))     ;; column
      ")" eol))

(-let* ((str "at Object.it (core/__tests__/Option.spec.ts:8:38)")
        ((whole file line column) (s-match zc-eval/jest-error-rx str)))
  (cl-assert (equal whole str))
  (cl-assert (equal file   "core/__tests__/Option.spec.ts"))
  (cl-assert (equal line   "8"))
  (cl-assert (equal column "38")))



(use-package compile
  :defer t
  :after evil

  :general
  (:states 'motion :keymaps 'compilation-mode-map
           "h" #'evil-backward-char)

  (:states 'normal :keymaps 'compilation-mode-map
           "g" #'recompile)

  :preface
  (defun zc-eval/colorize-compilation-buffer ()
    (unless (derived-mode-p 'rg-mode)
      (with-silent-modifications
        (ansi-color-apply-on-region compilation-filter-start (point)))))

  :hook (compilation-filter . zc-eval/colorize-compilation-buffer)

  :init
  (setq compilation-environment '("TERM=screen-256color")
        compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)

  :config
  (progn
    (setf (alist-get 'jest compilation-error-regexp-alist-alist)
          (list zc-eval/jest-error-rx 1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'jest)))

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
