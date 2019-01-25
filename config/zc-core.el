(eval-when-compile
  (require 'use-package))

(require 'general)


;; Environment

;; Ensure unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Shorten yes/no answers to y/n
(defalias #'yes-or-no-p #'y-or-n-p)
(defalias #'view-hello-file #'ignore)

;; Automatically update buffers when files change
(global-auto-revert-mode t)

;; Enable 'power user' features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Use UTF-8 everywhere.
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; General settings.
(setq-default fill-column 80
              indent-tabs-mode nil
              sentence-end-double-space t)

;; Evil breaks cursor settings when combined with hydra.
(setq-default cursor-in-non-selected-windows nil)

(setq apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      save-interprogram-paste-before-kill t
      ediff-window-setup-function 'ediff-setup-windows-plain

      save-place-file (concat paths-cache-directory "/places")
      url-cookie-file (concat paths-cache-directory "/cookies")
      recentf-save-file (concat paths-cache-directory "/recentf")
      bookmark-default-file (concat paths-cache-directory "/bookmarks")
      projectile-cache-file (concat paths-cache-directory "/projectile.cache")

      ;; Don't show the startup message
      inhibit-startup-message t


      ;; Prefer splitting windows horizontally
      split-height-threshold nil

      ;; Save backup files in the temporary directory
      make-backup-files nil
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      ;; Smooth scroll
      scroll-step            1
      scroll-conservatively  101
      scroll-margin          0
      scroll-preserve-screen-position t

      ;; Instantly display current keystrokes in mini buffer
      echo-keystrokes 0.02

      ;; Don't pollute directories with lockfiles
      create-lockfiles nil

      ;; Save clipboard strings into kill ring before replacing them
      save-interprogram-paste-before-kill t

      ;; Don't prompt when following symlinks to vc files.
      vc-follow-symlinks t

      ;; Don't try to ping things that look like domain names
      ffap-machine-p-known 'reject

      ;; Disable warnings from obsolete advice system, since these are
      ;; generally not actionable.
      ad-redefinition-action 'accept

      compilation-environment '("TERM=screen-256color")
      compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error

      comint-prompt-read-only t

      ;; Always focus on help windows
      help-window-select t

      ;; Prevent duplicated entries in the kill ring.
      kill-do-not-save-duplicates t

      ;; Don't confirm before killing subprocesses on exit.
      confirm-kill-processes nil

      apropos-do-all t
      doc-view-continuous t)


;; General key bindings

;; Reserved for Spacemacs style prefix key
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-l")) ;; downcase word

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-h c")   'describe-char)

(global-set-key (kbd "M-1")      'winum-select-window-1)
(global-set-key (kbd "M-2")      'winum-select-window-2)
(global-set-key (kbd "M-3")      'winum-select-window-3)
(global-set-key (kbd "M-4")      'winum-select-window-4)
(global-set-key (kbd "M-5")      'winum-select-window-5)

;; Make <escape> issue a keyboard-quit in as many situations as possible.
(define-key minibuffer-local-map            (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map         (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map    (kbd "<escape>") #'keyboard-escape-quit)


;; Build-in packages

(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package winner
  :config
  (progn
    (setq winner-boring-buffers
          '("*Completions*"
            "*Compile-Log*"
            "*inferior-lisp*"
            "*Fuzzy Completions*"
            "*Apropos*"
            "*Help*"
            "*cvs*"
            "*Buffer List*"
            "*Ibuffer*"
            "*esh command on file*"))
    (winner-mode t)))

(use-package ispell
  :init
  (setq ispell-really-hunspell t
        ispell-dictionary "english"
        ispell-program-name "hunspell"))


;; External Packages

(use-package pretty-hydra
  :straight (:host github :repo "jerrypnz/major-mode-hydra.el"))

(use-package avy
  :straight t
  :defer nil
  :config (setq avy-background t))

(use-package ace-window
  :straight t
  :general ("M-o" 'ace-window)
  :config
  (progn
    (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?8))
    (setq aw-background t)))

(use-package imenu-list
  :straight t
  :commands (imenu-list)
  :general (:states 'normal :keymaps 'imenu-list-major-mode-map
                    "q" #'quit-window))

(use-package smex
  :straight t
  :defer t
  :init (setq-default smex-history-length 32))

(use-package undo-tree
  :straight t
  :config
  (progn
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)
    (global-undo-tree-mode)))

(use-package which-key
  :straight t
  :defer t
  :config (setq which-key-idle-delay 0.3))



(require 'server)
(unless (server-running-p) (server-start))



(provide 'zc-core)
