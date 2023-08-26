;;; lang/zc-lisp/config.el -*- lexical-binding: t; -*-

;; Override doom-emacs default key bindings
(after! elisp-mode
  (map! :localleader
        :map (emacs-lisp-mode-map lisp-interaction-mode-map)

        (:prefix ("t" . "test")
         :desc "Run tests"         :n "t" #'ert
         :desc "Run all tests"     :n "a" (cmd! (ert t))
         :desc "Run failed tests"  :n "f" (cmd! (ert :failed)))

        (:prefix ("h" . "help")
         :desc "Show docs"         :n "h" #'zc-lisp/describe-at-point
         :desc "Describe function" :n "f" #'helpful-function
         :desc "Describe variable" :n "v" #'helpful-variable)))
