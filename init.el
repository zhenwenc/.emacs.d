;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Frederick Cai
;;
;; Author: Frederick Cai <frederick.cai@gmail.com>
;;
;;; Commentary:

;; This emacs setting is adapted from Chris Barrett's config here:
;; https://github.com/chrisbarrett/.emacs.d

;; Install libgccjit if necessary:
;;
;;   brew reinstall gcc libgccjit
;;
;; Install Emacs on MacOS:
;; https://github.com/railwaycat/homebrew-emacsmacport
;;
;;   brew install emacs-mac --with-emacs-sexy-icon
;;
;; https://github.com/d12frosted/homebrew-emacs-plus
;;
;;   brew install emacs-plus --with-modern-sexy-v2-icon

(when (version< emacs-version "26")
  (error "This requires Emacs 26 and above!"))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))



(defun zc/increase-gc-limit ()
  "Increase garbage collection limit to maximum value."
  (setq gc-cons-threshold most-positive-fixnum))

(defun zc/reset-gc-limit ()
  "Restore to defalut value, 16mb."
  (setq gc-cons-threshold (* 16 1024 1024)))

;; Defer GC further back to speed up startup
(zc/increase-gc-limit)
(add-hook 'emacs-startup-hook #'zc/reset-gc-limit)


;; Bootstrap straight.el package manager.

(defvar bootstrap-version)
(let ((bootstrap-version 5)
      (bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))

  ;; Improve bootstrap speed. The cache wile may grow quite
  ;; large overtime, if so run `straight-prune-build'.
  (with-no-warnings
    (setq straight-cache-autoloads t)
    (setq straight-check-for-modifications 'live)
    (setq straight-disable-native-compile nil))

  (load bootstrap-file nil 'nomessage))


;; Install basic packages

(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'lv)
(straight-use-package 'ht)
(straight-use-package 'noflet)
(straight-use-package 'memoize)
(straight-use-package 'general)
(straight-use-package 'posframe)
(straight-use-package 'el-patch)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

;; Must set before loading `use-package'.
;; Don't use `use-package-always-ensure' with straight!
(eval-when-compile
  (with-no-warnings
    (setq use-package-verbose t)))

(straight-use-package 'bind-map)
(straight-use-package 'use-package)

;; To avoid an obsolete version of Org get loaded.
(straight-use-package 'org)
(straight-use-package 'org-contrib)

(eval-when-compile
  (require 'use-package))


;; Setup Emacs paths

(require 'zc-paths
         (expand-file-name "config/zc-paths.el" user-emacs-directory))
(zc-paths/init-load-paths)
(zc-paths/init-local-modules)

(require 'zc-funcs)
(require 'zc-secret-funcs)


;; Load features

(use-package zc-theme)
(use-package zc-core)
(use-package zc-evil)
;; (use-package zc-ivy) ;; replaced with zc-completion
(use-package zc-completion)
(use-package zc-hydra)
(use-package zc-osx :if (eq system-type 'darwin))
(use-package zc-eval)
(use-package zc-editing)
(use-package zc-eshell)
(use-package zc-term)
(use-package zc-git)
(use-package zc-projectile)
(use-package zc-modeline)
(use-package zc-layout)
(use-package zc-smartparens)
(use-package zc-flycheck)
(use-package zc-flyspell)
(use-package zc-company)
(use-package zc-lsp)
(use-package zc-lisp)
(use-package zc-tools)
(use-package zc-treemacs)
(use-package zc-yasnippet)
(use-package zc-sql)
(use-package zc-org)
(use-package zc-web)
(use-package zc-lua)
(use-package zc-yaml)
(use-package zc-rust)
(use-package zc-scala)
(use-package zc-swift)
(use-package zc-kotlin)
(use-package zc-python)
(use-package zc-markdown)
(use-package zc-restclient)
(use-package zc-typescript)
(use-package zc-plantuml)
(use-package zc-mermaid)
(use-package zc-prodigy)
(use-package zc-prisma)
(use-package zc-help)


;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message ">>> Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))



(provide 'init)
