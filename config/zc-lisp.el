(eval-when-compile
  (require 'use-package))

(require 'el-patch)
(require 'general)
(require 'zc-hydra-funcs)

(defvar calculate-lisp-indent-last-sexp)

(autoload 'sp-up-sexp "smartparens")
(autoload 'sp-point-in-string-or-comment "smartparens")
(autoload 'helpful-callable "helpful")
(autoload 'helpful-variable "helpful")


;; Redefinitions

;; Fix the indentation of keyword lists in Emacs Lisp.
;;
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
;;
;; https://github.com/Fuco1/.emacs.d
;; http://emacs.stackexchange.com/q/10230/12534
(el-patch-defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (el-patch-let
      (($cond (and (elt state 2)
                   (el-patch-wrap 1 1
                     (or (not (looking-at "\\sw\\|\\s_"))
                         (looking-at ":")))))
       ($then (progn
                (if (not (> (save-excursion (forward-line 1) (point))
                            calculate-lisp-indent-last-sexp))
                    (progn (goto-char calculate-lisp-indent-last-sexp)
                           (beginning-of-line)
                           (parse-partial-sexp (point)
                                               calculate-lisp-indent-last-sexp 0 t)))
                ;; Indent under the list or under the first sexp on the same
                ;; line as calculate-lisp-indent-last-sexp.  Note that first
                ;; thing on that line has to be complete sexp since we are
                ;; inside the innermost containing sexp.
                (backward-prefix-chars)
                (current-column)))
       ($else (let ((function (buffer-substring (point)
                                                (progn (forward-sexp 1) (point))))
                    method)
                (setq method (or (function-get (intern-soft function)
                                               'lisp-indent-function)
                                 (get (intern-soft function) 'lisp-indent-hook)))
                (cond ((or (eq method 'defun)
                           (and (null method)
                                (> (length function) 3)
                                (string-match "\\`def" function)))
                       (lisp-indent-defform state indent-point))
                      ((integerp method)
                       (lisp-indent-specform method state
                                             indent-point normal-indent))
                      (method
                       (funcall method indent-point state))))))
    (let ((normal-indent (current-column))
          (el-patch-add (orig-point (point))))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (el-patch-swap
        (if $cond
            ;; car of form doesn't seem to be a symbol
            $then
          $else)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ($cond $then)
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t $else))))))



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
