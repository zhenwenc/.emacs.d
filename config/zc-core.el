(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'zc-paths)


;; Environment

;; Ensure unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Shorten yes/no answers to y/n
(defalias #'yes-or-no-p #'y-or-n-p)
(defalias #'view-hello-file #'ignore)

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

(setq-default
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 save-interprogram-paste-before-kill t
 ediff-window-setup-function 'ediff-setup-windows-plain

 save-place-file       (concat paths-cache-dir "places")
 url-cookie-file       (concat paths-cache-dir "cookies")
 recentf-save-file     (concat paths-cache-dir "recentf")
 bookmark-default-file (concat paths-cache-dir "bookmarks")

 ;; General settings.
 indent-tabs-mode nil
 sentence-end-double-space t

 ;; Evil breaks cursor settings when combined with hydra.
 cursor-in-non-selected-windows nil

 ;; These are generated custom configurations, stop it
 ;; from polluting our init.el file!
 custom-file           (concat paths-cache-dir "custom.el")

 ;; Quiet startup
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message nil

 ;; Disable useless features
 frame-title-format nil

 ;; Prefer splitting windows horizontally
 split-height-threshold nil

 ;; Save backup files in the temporary directory
 make-backup-files               nil
 backup-directory-alist         `((".*" . ,(concat paths-cache-dir "backup/")))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 auto-save-list-file-name        (concat paths-cache-dir "autosave")
 auto-save-list-file-prefix      (concat paths-cache-dir "autosave/.saves-")
 auto-save-no-message            t

 ;; Tramp
 tramp-default-method            "ssh"
 tramp-persistency-file-name     (concat paths-cache-dir "tramp")
 tramp-auto-save-directory       (concat paths-cache-dir "tramp-autosave")
 tramp-backup-directory-alist    backup-directory-alist
 tramp-histfile-override         "/tmp/.tramp" ; shhh!

 ;; Smooth scroll
 scroll-step                     1
 scroll-conservatively           101
 scroll-margin                   0
 scroll-up-aggressively          0.01
 scroll-down-aggressively        0.01
 scroll-preserve-screen-position t

 ;; Improve page scrolling experience
 ;; https://emacs.stackexchange.com/questions/31402
 ;;
 ;; FIXME: Invalid time specification
 ;; jit-lock-defer-time t

 ;; Reduce cursor movement lag
 ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
 auto-window-vscroll nil

 ;; Instantly display current keystrokes in mini buffer
 echo-keystrokes 0.02

 ;; Don't pollute directories with lockfiles
 create-lockfiles nil

 ;; Save clipboard strings into kill ring before replacing them
 save-interprogram-paste-before-kill t

 ;; Don't prompt when following symlinks to vc files.
 vc-follow-symlinks t
 vc-handled-backends '(Git) ; don't border trying others!

 ;; Don't try to ping things that look like domain names
 ffap-machine-p-known 'reject

 ;; Disable warnings from obsolete advice system, since these are
 ;; generally not actionable.
 ad-redefinition-action 'accept

 comint-prompt-read-only t

 ;; Always focus on help windows
 help-window-select t

 ;; Prevent duplicated entries in the kill ring.
 kill-do-not-save-duplicates t

 ;; Don't confirm before killing subprocesses on exit.
 confirm-kill-processes nil

 ;; Reduce `line-move' CUP usage overhead.
 line-move-visual nil

 ;; Display Work Clock with `display-time-world'
 display-time-world-time-format "%a %d %b %T %Z"
 zoneinfo-style-world-list '(("Pacific/Auckland" "New Zealand Auckland")
                             ("Australia/Sydney" "Australia   Sydney")
                             ("Asia/Shanghai"    "China       Beijing")
                             ("Europe/London"    "Europe      London")
                             ("America/New_York" "America     New York"))

 apropos-do-all t
 doc-view-continuous t)

 ;; Do not popup the *Warnings* buffer
(setq-default native-comp-async-report-warnings-errors 'silent)
(setq-default warning-minimum-level :error)


;; Performance

;; Garbage collection makes emacs incredibly slow, disabling
;; it temporarily to speed up minibuffer operations that tend
;; to be memory heavy like fuzzy searches.
(add-hook 'minibuffer-setup-hook #'zc/max-gc-limit)
(add-hook 'minibuffer-exit-hook  #'zc/reset-gc-limit)

;; Improve the performance of rendering long lines.
(setq-default bidi-display-reordering nil)


;; General key bindings

;; Reserved for Spacemacs style prefix key
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-l")) ;; downcase word

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-h c")   'describe-char)
(global-set-key (kbd "s-u")     'revert-buffer)
(global-set-key (kbd "C-c R")   'profiler-report)

;; Make <escape> issue a keyboard-quit in as many situations as possible.
(define-key minibuffer-local-map            (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map         (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map    (kbd "<escape>") #'keyboard-escape-quit)

(general-define-key :states  '(normal visual motion)
                    :keymaps '(special-mode-map messages-buffer-mode-map)
                    "SPC"    #'zc-main-hydra/body)

;; We have hydra to adjust the text scale.
(general-define-key :states 'normal :keymaps 'view-mode-map "0" nil)


;; Build-in packages

;; Delete selection if insert someting
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically update buffers when files change
(use-package autorevert
  :ensure nil
  :init
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package conf-mode
  :mode ("\\.ovpn\\'" . conf-space-mode))


;; External Packages

(use-package avy
  :straight t
  :defer nil
  :config (setq avy-background t))

(use-package imenu-list
  :straight t
  :commands (imenu-list)
  :general (:states 'normal :keymaps 'imenu-list-major-mode-map
            "d" #'imenu-list-display-entry
            "r" #'imenu-list-refresh
            "q" #'quit-window)
  :config
  (setq imenu-list-focus-after-activation nil
        imenu-list-auto-resize t))

(use-package smex
  :straight t
  :defer t
  :defines (smex-history-length smex-save-file)
  :init
  (setq smex-history-length 32
        smex-save-file (concat paths-cache-dir "smex-items")))

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

;; Provides intelligent sorting and filtering for candidates.
(use-package prescient
  :straight t
  :config
  (setq prescient-save-file (concat paths-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

;; Watches for long pauses and reports
(use-package explain-pause-mode
  :straight (:host github :repo "lastquestion/explain-pause-mode"))



(use-package server
  :when (display-graphic-p)
  :defer 1
  :config
  (require 'server)
  (unless (server-running-p) (server-start)))



(provide 'zc-core)
