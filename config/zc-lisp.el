(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'zc-hydra-funcs)

(autoload 'sp-up-sexp "smartparens")
(autoload 'sp-point-in-string-or-comment "smartparens")
(autoload 'helpful-callable "helpful")
(autoload 'helpful-variable "helpful")



;; http://www.sugarshark.com/elisp/init/lisp.el.html
(defun zc-lisp/describe-at-point ()
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



(use-package elisp-mode
  :straight nil

  ;; Prefer xref-find than dump-jump for elisp
  :general (:states 'normal :keymaps 'emacs-lisp-mode-map
                    "M-,"   #'pop-tag-mark
                    "M-."   #'xref-find-definitions
                    "C-M-." #'xref-find-apropos)

  :config
  (zc-hydra/major-mode-define emacs-lisp-mode
    ("Basic"
     (("!" ielm "REPL"))

     "Compile"
     (("cc" emacs-lisp-byte-compile "compile"))

     "Eval & Expand"
     (("eb" eval-buffer "buffer")
      ("ef" eval-defun "defun")
      ("er" eval-region "region")
      ("ee" zc-lisp/eval-current-form-sp "current form")
      ("em" pp-macroexpand-last-sexp "expand macro"))

     "Test"
     (("tt" ert "prompt")
      ("ta" (ert t) "all")
      ("tf" (ert :failed) "failed"))

     "Doc"
     (("hh" zc-lisp/describe-at-point "doc at point")
      ("hf" helpful-function "desc function")
      ("hv" helpful-variable "desc variable")))))



(provide 'zc-lisp)
