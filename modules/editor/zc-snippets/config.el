;;; editor/zc-snippets/config.el -*- lexical-binding: t; -*-

(with-eval-after-load 'yasnippet
  ;; Load our own snippets
  (add-to-list 'yas-snippet-dirs (concat doom-user-dir "snippets") t))
