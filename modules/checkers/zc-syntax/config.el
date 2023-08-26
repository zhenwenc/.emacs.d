;;; checkers/zc-syntax/config.el -*- lexical-binding: t; -*-

(defun zc-flycheck/disable-checkers (&rest checkers)
  "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
  (unless (boundp 'flycheck-disabled-checkers)
    (setq flycheck-disabled-checkers nil))
  (make-local-variable 'flycheck-disabled-checkers)
  (dolist (checker checkers)
    (cl-pushnew checker flycheck-disabled-checkers)))
