;; -*- no-byte-compile: t; -*-
;;; lang/zc-typescript/packages.el

(when (< emacs-major-version 29)
  (package! typescript-mode))

(package! tide)

(package! js2-mode) ;; borrow utility functions
