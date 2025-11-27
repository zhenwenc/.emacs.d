;;; tools/zc-lsp/config.el -*- lexical-binding: t; -*-

(after! eglot
  ;; Doom uses `eldoc-hint' by default, which look quite odd in the eldoc area.
  ;; The `mode-line' option actually makes it hidden.
  (setq eglot-code-action-indications '(mode-line)))
