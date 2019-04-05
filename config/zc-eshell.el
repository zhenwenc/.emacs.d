(eval-when-compile
  (require 'use-package))

(require 'zc-paths)
(require 'zc-eshell-funcs)

(autoload 'evil-define-key "evil-core")



(defvar zc-eshell/aliases
  '(("q"     "exit")
    ("d"     "docker $*")
    ("j"     "eshell/z $*")
    ("f"     "find-file $1")
    ("ag"    "ag --color $*")
    ("rg"    "rg --color=always $*")
    ("l"     "ls -lh $*")
    ("ll"    "ls -lah $*")
    ("la"    "ls -lAh $*")
    ("tf"    "terraform $*")
    ("tree"  "tree -I 'target|node_modules' $*")
    ("clear" "clear-scrollback"))
  "An alist of default eshell aliases. Any alias defined in
`eshell-aliases-file' will be overwritten.")



(use-package shrink-path
  :straight t
  :commands (shrink-path-file))

(use-package eshell
  :commands (eshell)

  :defines (eshell-command-aliases-list
            eshell-visual-commands
            eshell-visual-subcommands
            eshell-hist-ignoredups
            eshell-glob-case-insensitive
            eshell-error-if-no-glob
            eshell-prompt-regexp
            eshell-prompt-function)

  :functions (eshell-write-aliases-list)

  :preface
  (defun zc-eshell/init-aliases ()
    (setq eshell-command-aliases-list zc-eshell/aliases))

  :preface
  (defun zc-eshell/init-evil ()
    "Ensures the point is on the prompt when changing to
replace or insert mode."
    (dolist (hook '(evil-replace-state-entry-hook
                    evil-insert-state-entry-hook))
      (remove-hook hook 'evil-collection-eshell-next-prompt-on-insert t)
      (add-hook hook 'zc-eshell/goto-prompt-on-insert nil t)))

  :preface
  (defun zc-eshell/init-keymap ()
    "Overwrite keybinding defined in `evil-collection'."
    (evil-define-key 'normal eshell-mode-map
      (kbd "<return>") #'zc-eshell/goto-end-of-prompt
      (kbd "q")        #'delete-window
      (kbd "C-a x")    #'zc-eshell/kill-and-close
      (kbd "C-a z")    #'delete-window)
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-e")      #'end-of-line
      (kbd "C-p")      #'eshell-previous-input
      (kbd "C-n")      #'eshell-next-input
      (kbd "C-a C-a")  #'eshell-bol
      (kbd "C-a x")    #'zc-eshell/kill-and-close
      (kbd "C-a z")    #'delete-window))

  :hook
  ((eshell-mode            . zc-eshell/init-evil)
   (eshell-exit            . zc-eshell/cleanup)
   (eshell-alias-load      . zc-eshell/init-aliases)
   (eshell-first-time-mode . zc-eshell/init-keymap))

  :init
  (setq eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-directory-name (concat paths-cache-dir "/eshell")

        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'zc-eshell/prompt

        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  :config
  (progn
    ;; We configure our eshell aliases via `zc-eshell/aliases'.
    (advice-add #'eshell-write-aliases-list :override #'ignore)

    ;; Visual commands require a proper terminal. Eshell can't
    ;; handle that, so delegate these commands to a term buffer.
    (with-eval-after-load 'em-term
      (dolist (cmd '("tmux" "htop" "zsh" "vim"))
        (add-to-list 'eshell-visual-commands cmd))

      (dolist (cmd '(("yarn" "release")))
        (add-to-list 'eshell-visual-subcommands cmd)))))

(use-package eshell-z
  :straight t
  :after eshell
  :config
  ;; Preferentially use zsh's db if it exists
  (unless (file-exists-p eshell-z-freq-dir-hash-table-file-name)
    (setq eshell-z-freq-dir-hash-table-file-name
          (expand-file-name "z" eshell-directory-name))))



(provide 'zc-eshell)
