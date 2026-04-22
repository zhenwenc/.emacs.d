;;; core/zc-prelude/config.el -*- lexical-binding: t; -*-

(use-package! ht)

;; HACK Doom's black magic is problematic!
;;
;; Disable `so-long' entirely. Doom enables `global-so-long-mode' on
;; `doom-first-file-hook' in `lisp/doom-editor.el'.
(remove-hook 'doom-first-file-hook #'global-so-long-mode)
(after! so-long (global-so-long-mode -1))
