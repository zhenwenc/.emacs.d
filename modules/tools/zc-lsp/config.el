;;; tools/zc-lsp/config.el -*- lexical-binding: t; -*-

(after! eglot
  ;; Doom uses `eldoc-hint' by default, which look quite odd in the eldoc area.
  ;; The `mode-line' option actually makes it hidden.
  (setq eglot-code-action-indications '(mode-line))

  ;; Leave some major modes to their standard module.
  (defun zc-lsp/maybe-disable-eglot ()
    (when (eq major-mode 'json-mode)
      (setq eglot--managed-mode nil)))
  (add-hook 'eglot-managed-mode-hook #'zc-lsp/maybe-disable-eglot))
