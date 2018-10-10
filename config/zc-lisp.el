(eval-when-compile
  (require 'use-package))

(require 'zc-hydra-funcs)

(autoload 'sp-up-sexp "smartparens")
(autoload 'sp-point-in-string-or-comment "smartparens")



;; http://www.sugarshark.com/elisp/init/lisp.el.html
(defun zc-lisp/describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever. we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (helpful-callable sym))
          ((setq sym (variable-at-point)) (helpful-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-called-at-point)) (helpful-callable sym)))))

(defun zc-lisp/eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of parentheses.
Will exit any strings and/or comments first. An optional ARG can be
used which is passed to `sp-up-sexp'to move out of more than one sexp.

Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun zc-lisp/macroexpand-point (sexp)
  "Expand the macro at point and display on temporary buffer."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*"
    (emacs-lisp-mode)))



(zc-hydra/major-mode-define emacs-lisp-mode
  ("Basic"
   (("!" ielm "REPL"))

   "Compile"
   (("cc" emacs-lisp-byte-compile "compile")
    ("cl" auto-compile-display-log "compile log"))

   "Eval"
   (("eb" eval-buffer "buffer")
    ("ef" eval-defun "defun")
    ("er" eval-region "region")
    ("ee" zc-lisp/eval-current-form-sp "current form"))

   "Macro"
   (("me" zc-lisp/macroexpand-point "expand"))

   "Test"
   (("tt" ert "prompt")
    ("ta" (ert t) "all")
    ("tf" (ert :failed) "failed"))

   "Doc"
   (("hh" zc-lisp/describe-foo-at-point "doc at point")
    ("hf" describe-function)
    ("hv" describe-variable))))

;; Prefer xref-find than dump-jump for elisp
(define-key emacs-lisp-mode-map (kbd "M-.") #'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-,") #'xref-find-apropos)

(provide 'zc-lisp)
