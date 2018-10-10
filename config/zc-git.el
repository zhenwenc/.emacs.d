(eval-when-compile
  (require 'use-package))

(use-package magit
  :straight t
  :config
  (setq magit-repository-directories
        '(("~/code/"     . 2)
          ("~/.emacs.d/" . 0))))

;; Reconfigures magit keybindings to better support evil.

(use-package evil-magit
  :straight t
  :after (:and magit evil-common)
  :config (evil-magit-init))

;; Interfaces to GitHub integrated into Magit

(use-package magithub
  :straight t
  :after magit
  :defer t
  :config
  (progn
    (require 'magithub-completion)
    (magithub-feature-autoinject t)
    (setq magithub-clone-default-directory "~/code/github")))

;; Interactively step forward and backwards through a
;; buffer's git versions.

(use-package git-timemachine
  :straight t
  :defer t)

(use-package vc-annotate
  :commands (vc-annotate)
  :general
  (:states 'normal :keymaps 'vc-annotate-mode-map
   "n" 'vc-annotate-next-revision
   "f" 'vc-annotate-next-revision
   "p" 'vc-annotate-prev-revision
   "b" 'vc-annotate-prev-revision
   "." 'vc-annotate-working-revision))

(provide 'zc-git)
