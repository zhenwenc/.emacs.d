(eval-when-compile
  (require 'use-package))

(require 'zc-eval-funcs)
(require 'zc-hydra-funcs)



(defconst zc-eval/jest-error-rx
  (rx bol "at" space (? (: (+? nonl) "("))
      (group (+? nonl)) ":" ;; filename
      (group (+ digit)) ":" ;; line
      (group (+ digit))     ;; column
      (? ")") eol))

(defconst zc-eval/jest-console-rx
  (rx bol "console." (+ word) (+ space)
      (group (+? nonl)) ":" ;; filename
      (group (+ digit))     ;; line
      (or eol (not (any ":")))))

(-let* ((str "at Object.it (core/__tests__/Option.spec.ts:8:38)")
        ((whole file line column) (s-match zc-eval/jest-error-rx str)))
  (cl-assert (equal whole str))
  (cl-assert (equal file   "core/__tests__/Option.spec.ts"))
  (cl-assert (equal line   "8"))
  (cl-assert (equal column "38")))

(-let* ((str "at core/__tests__/Option.spec.ts:8:38")
        ((whole file line column) (s-match zc-eval/jest-error-rx str)))
  (cl-assert (equal whole str))
  (cl-assert (equal file   "core/__tests__/Option.spec.ts"))
  (cl-assert (equal line   "8"))
  (cl-assert (equal column "38")))

(-let* ((str "console.info core/__tests__/Option.spec.ts:107")
        ((whole file line) (s-match zc-eval/jest-console-rx str)))
  (cl-assert (equal whole str))
  (cl-assert (equal file   "core/__tests__/Option.spec.ts"))
  (cl-assert (equal line   "107")))



(use-package compile
  :defer t
  :after evil

  :general
  (:states 'normal :keymaps 'compilation-mode-map
   "h"   #'evil-backward-char)

  (:states 'insert :keymaps 'compilation-mode-map
   "a"   #'zc-eval/compilation-send-self
   "f"   #'zc-eval/compilation-send-self
   "RET" #'zc-eval/compilation-send-self

   :predicate '(not (derived-mode-p 'rustic-compilation-mode))
   "g"   #'recompile)

  :hydra
  ((:mode compilation-mode)
   ("Basic"
    (("i" zc-eval/compilation-send-input "send input")
     ("I" zc-eval/compilation-toggle-comint "toggle comint"))))

  :hook ((compilation-mode   . zc-eval/compilation-resize-buffer)
         (compilation-filter . zc-eval/compilation-colorize-buffer))

  :config
  (defun zc-eval/compilation-colorize-buffer ()
    "Apply ansi codes to the compilation buffers."
    (unless (derived-mode-p 'rg-mode)
      (with-silent-modifications
        ;; FIXME: Maybe try the alternative `xterm-color'.
        (ansi-color-apply-on-region compilation-filter-start (point))
        ;; FIXME: Are there any proper solution to remove
        ;;        these unwanted carriage characters?
        ;;
        ;; Run rust cargo test all to reproduce the issue.
        (replace-string "" "" nil compilation-filter-start (point)))))

  (defun zc-eval/compilation-resize-buffer ()
    "Reduce text size for better visibility."
    (text-scale-set -1))

  :config
  (setq compilation-environment '("TERM=screen-256color")
        compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error
        compilation-message-face 'font-lock-comment-face)

  (-each `((jest-error   ,(list zc-eval/jest-error-rx   1 2 3 1))
           (jest-console ,(list zc-eval/jest-console-rx 1 2 nil 1)))
    (-lambda ((type elt))
      (setf (alist-get type compilation-error-regexp-alist-alist) elt)
      (add-to-list 'compilation-error-regexp-alist type)))

  (advice-add #'projectile-read-command :override
              #'zc-eval/projectile-read-command))



(provide 'zc-eval)
