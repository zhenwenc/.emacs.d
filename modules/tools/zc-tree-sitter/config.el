;;; tools/zc-tree-sitter/config.el -*- lexical-binding: t; -*-

(with-eval-after-load 'treesit
  ;; Reduce the doom default level 4 to reduce distraction
  (setq treesit-font-lock-level 3))
