(eval-when-compile
  (require 'use-package))

(use-package magit
  :straight t
  :config
  (setq magit-repository-directories
        '(("~/.emacs.d/" . 0)
          ("~/org/"      . 0)
          ("~/code/"     . 2)
          ("~/dotfiles/" . 0))))

;; Transient commands (previously known as magit-popup)

(use-package transient
  :straight t
  :init
  (setq transient-levels-file  (concat paths-cache-directory "/transient/levels.el")
        transient-values-file  (concat paths-cache-directory "/transient/values.el")
        transient-history-file (concat paths-cache-directory "/transient/history.el")))

;; Show source files' todos in Magit status buffer

(use-package magit-todos
  :straight t
  :hook (magit-mode . magit-todos-mode))

;; Reconfigures magit keybindings to better support evil.

(use-package evil-magit
  :straight t
  :after (:and magit evil-common)
  :config (evil-magit-init))

;; Interfaces to GitHub integrated into Magit

;; (use-package magithub
;;   :straight t
;;   :after magit
;;   :preface
;;   (setq magithub-dir (concat paths-cache-directory "/magithub"))
;;   :init
;;   (setq magithub-clone-default-directory "~/code/github"
;;         magithub-preferred-remote-method 'clone_url)
;;   :config
;;   (progn
;;     ;; See `magithub-feature-list' for available features.
;;     (magithub-feature-autoinject
;;      '(pull-request-merge commit-browse completion))))

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
