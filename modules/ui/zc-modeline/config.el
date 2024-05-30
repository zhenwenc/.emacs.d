(use-package! doom-modeline
  :config
  ;; Shorten buffer file name
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)

  ;; Define custom modeline
  ;; https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline.el
  (doom-modeline-def-modeline 'zc-main
    '(eldoc bar workspace-name window-number modals matches
      follow buffer-info remote-host buffer-position
      word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip
      irc mu4e gnus github debug repl lsp
      minor-modes input-method indent-info
      buffer-encoding major-mode process vcs check time))

  (defun zc/doom-modeline-set-default-modeline-h ()
    (doom-modeline-set-modeline 'zc-main 'default)))
