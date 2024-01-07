(after! evil
  (setq evil-echo-state nil)
  ;; Use native keybindings, prevent overriding `C-t' and `C-d' keys
  (setq evil-disable-insert-state-bindings t)
  ;; Makes v$ consistent with $ used as a motion, do not move as far
  ;; as to include the char at eol
  (setq evil-v$-excludes-newline t)
  ;; Do not add the replaced text into kill ring
  (setq evil-kill-on-visual-paste nil)

  ;; Motion commands navigate by visual lines
  ;; - `evil-next-line'     => `evil-next-visual-line'
  ;; - `evil-previous-line' => `evil-previous-visual-line'
  (setq evil-respect-visual-line-mode t)
  ;; FIXME Remove override once `evil-respect-visual-line-mode' is working
  ;; DISABLED: The behaviour so weird!
  ;; (map!
  ;;  :nvm "j" #'evil-next-visual-line
  ;;  :nvm "k" #'evil-previous-visual-line)

  ;; Configure cursors.
  (setq evil-motion-state-cursor '("plum3" box))
  (setq evil-visual-state-cursor '("gray" (hbar . 2)))
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
  (setq evil-emacs-state-cursor  '("SkyBlue2" hbar))

  ;; Override doom's default search module
  ;; https://emacs.stackexchange.com/a/24913
  (evil-select-search-module 'evil-search-module 'isearch)

  ;; Configure initial state for modes
  (evil-set-initial-state 'minibuffer-mode          'emacs)
  (evil-set-initial-state 'diff-mode                'motion)
  (evil-set-initial-state 'shell-mode               'normal)
  (evil-set-initial-state 'ibuffer-mode             'motion)
  (evil-set-initial-state 'prodigy-mode             'motion)
  (evil-set-initial-state 'process-menu-mode        'motion)
  (evil-set-initial-state 'org-agenda-mode          'motion)
  (evil-set-initial-state 'compilation-mode         'motion)
  (evil-set-initial-state 'tide-references-mode     'emacs)
  (evil-set-initial-state 'tide-project-errors-mode 'normal))

(use-package! evil-escape
  :config
  (setq evil-escape-delay 0.2)
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-excluded-major-modes '(help-mode
                                           treemacs-mode
                                           ibuffer-mode
                                           image-mode
                                           magit-mode
                                           magit-diff-mode
                                           magit-cherry-mode
                                           magit-process-mode
                                           magit-log-mode
                                           magit-refs-mode
                                           magit-status-mode)))

(use-package! evil-iedit-state
  :commands (evil-iedit-state/iedit-mode)
  :config
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil))
