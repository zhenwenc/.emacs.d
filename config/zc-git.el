(eval-when-compile
  (require 'use-package))

(use-package magit
  :straight t

  :general (:states 'normal :keymaps 'magit-status-mode-map
            "q" #'magit-mode-bury-buffer)

  :config
  (setq magit-repository-directories
        '(("~/.emacs.d/" . 0)
          ("~/code/"     . 2)
          ("~/notes/"    . 0)
          ("~/dotfiles/" . 0)))

  ;; Display magit buffer in fullframe
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Show source files' todos in Magit status buffer

(use-package magit-todos
  :straight t
  :disabled t
  :hook (emacs-startup . magit-todos-mode))

;; Reconfigures magit keybindings to better support evil.

(use-package evil-magit
  :straight t
  :after (:and magit evil-common)
  :config (evil-magit-init))

;; Transient commands (previously known as magit-popup)

(use-package transient
  :straight t
  :general (:keymaps 'transient-map
            "<escape>" #'transient-quit-one
            "q"        #'transient-quit-one)
  :init
  (setq transient-levels-file  (concat paths-cache-dir "transient/levels.el")
        transient-values-file  (concat paths-cache-dir "transient/values.el")
        transient-history-file (concat paths-cache-dir "transient/history.el")))

;; Interfaces to GitHub integration

(use-package forge
  :straight t
  :after magit
  :commands magit-status
  :init
  (setq forge-database-file (concat paths-cache-dir "forge/database.sqlite")))

;; Interactively step forward and backwards through a
;; buffer's git versions.

(use-package git-timemachine
  :straight t
  :defer t)

;; Browse github/gitlab/bitbucket page

(use-package browse-at-remote
  :straight t
  :defer t)

(use-package vc-annotate
  :commands (vc-annotate)
  :general (:states 'normal :keymaps 'vc-annotate-mode-map
            "n" 'vc-annotate-next-revision
            "f" 'vc-annotate-next-revision
            "p" 'vc-annotate-prev-revision
            "b" 'vc-annotate-prev-revision
            "." 'vc-annotate-working-revision))

(provide 'zc-git)
