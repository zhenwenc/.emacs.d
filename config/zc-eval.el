(eval-when-compile
  (require 'use-package))

(require 'zc-eval-funcs)
(require 'zc-hydra-funcs)



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
  (:states 'normal :keymaps 'compilation-mode-map
           "h"   #'evil-backward-char)

  (:states 'insert :keymaps 'compilation-mode-map
           "g"   #'recompile
           "a"   #'zc-eval/compilation-send-self
           "f"   #'zc-eval/compilation-send-self
           "RET" #'zc-eval/compilation-send-self)

  :preface
  (defun zc-eval/compilation-colorize-buffer ()
    "Apply ansi codes to the compilation buffers."
    (unless (derived-mode-p 'rg-mode)
      (with-silent-modifications
        (ansi-color-apply-on-region compilation-filter-start (point)))))

  :preface
  (defun zc-eval/compilation-resize-buffer ()
    "Reduce text size for better visibility."
    (text-scale-set -1))

  :hook ((compilation-mode   . zc-eval/compilation-resize-buffer)
         (compilation-filter . zc-eval/compilation-colorize-buffer))

  :init
  (setq compilation-environment '("TERM=screen-256color")
        compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error
        compilation-message-face 'font-lock-comment-face)

  :config
  (progn
    (setf (alist-get 'jest compilation-error-regexp-alist-alist)
          (list zc-eval/jest-error-rx 1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'jest)

    (advice-add #'projectile-read-command :override
                #'zc-eval/projectile-read-command)))

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



(zc-hydra/major-mode-define compilation-mode
  ("Basic"
   (("i" zc-eval/compilation-send-input "send input")
    ("I" zc-eval/compilation-toggle-comint "toggle comint"))))



(provide 'zc-eval)
