;; -*- no-byte-compile: t; -*-
;;; lang/zc-typescript/packages.el

(when (< emacs-major-version 29)
  (package! typescript-mode)
  (package! js2-mode))

(package! tide)
