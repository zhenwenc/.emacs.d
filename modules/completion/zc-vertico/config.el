;;; completion/vertico/config.el -*- lexical-binding: t; -*-

(after! consult
  ;; By default, it searches from the current line, which results in a
  ;; very confusing list of candidates, where items in the bottom are
  ;; appearing on the top of the buffer.
  (setq consult-line-start-from-top t))

;; Recenter after navigated to a new position, keep you focused
(add-hook! '(imenu-after-jump-hook
             better-jumper-post-jump-hook
             consult-after-jump-hook
             dumb-jump-after-jump-hook)
           #'recenter)
