(use-package! doom-modeline
  :config
  ;; Shorten buffer file name
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)

  ;; Define custom modeline
  (doom-modeline-def-modeline 'zc-main
    '(eldoc bar workspace-name window-number modals matches
      follow buffer-info remote-host buffer-position
      word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip
      irc mu4e gnus github debug repl lsp
      minor-modes input-method indent-info
      buffer-encoding major-mode process vcs checker time))

  (defun zc/doom-modeline-set-default-modeline-h ()
    (doom-modeline-set-modeline 'zc-main 'default)))
