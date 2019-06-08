(eval-when-compile
  (require 'use-package))

(require 'company)
(require 'general)
(require 'straight)
(require 'zc-hydra-funcs)



(defun zc-ensime/setup ()
  "Setup ENSIME."
  ;; ensure the file exists before starting `ensime-mode'
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode 1))
   ((buffer-file-name)
    (add-hook 'after-save-hook 'ensime-mode nil t))))

(defun zc-ensime/ensime-refactor-accept ()
  (interactive)
  (with-no-warnings (funcall continue-refactor))
  (ensime-popup-buffer-quit-function))

(defun zc-ensime/ensime-refactor-cancel ()
  (interactive)
  (with-no-warnings (funcall cancel-refactor))
  (ensime-popup-buffer-quit-function))

(defun zc-ensime/ensime-gen-and-restart()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (sbt-command ";ensimeConfig;ensimeConfigProject")
    (ensime-shutdown)
    (ensime)))

(defun zc-ensime/ensime-inf-eval-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (ensime-inf-eval-buffer)
  (ensime-inf-switch)
  (evil-insert-state))

(defun zc-ensime/ensime-inf-eval-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (ensime-inf-switch)
  (ensime-inf-eval-region start end)
  (evil-insert-state))

;; HACK: Manually reset some company variables that were set by Ensime
;; TODO: Revisit company backends for ensime
(defun zc-ensime/set-company-variables (&rest _)
  (unless (ensime-connected-p)
    (setq-local company-idle-delay 0.5)
    (setq-local company-minimum-prefix-length
                (default-value 'company-minimum-prefix-length))))


(use-package ensime
  :straight t
  :defer t

  :general

  ;; Never ever let ensime check the whole project
  (:keymaps 'ensime-mode-map
            "C-c C-c a" #'ensime-show-all-errors-and-warnings)

  (:states 'insert :keymaps 'ensime-mode-map
           "M-." #'ensime-edit-definition
           "M-," #'ensime-pop-find-definition-stack)

  (:states 'normal :keymaps 'ensime-mode-map
           "M-." #'ensime-edit-definition
           "M-," #'ensime-pop-find-definition-stack
           "·"   #'ensime-edit-definition-other-window
           "RET" #'ensime-type-at-point)

  (:states 'normal :keymaps 'ensime-popup-buffer-map
           "q"   #'ensime-popup-buffer-quit-function)

  (:states 'normal :keymaps 'ensime-inspector-mode-map
           "M-." #'ensime-inspector-browse-source
           "K"   #'ensime-inspector-browse-doc
           "q"   #'ensime-popup-buffer-quit-function
           ","   #'ensime-inspector-backward-page
           "."   #'ensime-inspector-forward-page
           "^"   #'ensime-inspector-backward-page)

  (:states 'normal :keymaps 'ensime-refactor-info-map
           "q"   #'zc-ensime/ensime-refactor-cancel
           "c"   #'zc-ensime/ensime-refactor-accept
           "RET" #'zc-ensime/ensime-refactor-accept)

  (:states 'normal :keymaps 'ensime-compile-result-map
           "g"   #'ensime-show-all-errors-and-warnings
           "n"   #'forward-button
           "N"   #'backward-button)

  :hook (scala-mode . zc-ensime/setup)

  :init
  (progn
    (setq ensime-startup-dirname (concat paths-cache-dir "ensime/"))

    )

  :config
  (progn
    (setq ensime-startup-snapshot-notification nil)
    (setq ensime-startup-notification nil)
    (setq ensime-auto-generate-config nil)
    (setq ensime-implicit-gutter-icons nil)
    (setq ensime-tooltip-hints nil)
    (setq ensime-search-interface 'ivy)

    ;; Make ENSIME faster
    (setq ensime-sem-high-enabled-p nil)
    (setq ensime-typecheck-when-idle nil)

    (setq ensime-goto-test-config-defaults
          (list :test-class-names-fn #'ensime-goto-test--test-class-names
                :test-class-suffixes '("Test" "Tests"
                                       "IntTest" "IntTests"
                                       "IntegrationTest" "IntegrationTests"
                                       "Spec" "Specs"
                                       "Specification" "Specifications"
                                       "Prop" "Props" "Property" "Properties"
                                       "Check" "Checks")
                :impl-class-name-fn #'ensime-goto-test--impl-class-name
                :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
                :is-test-dir-fn #'ensime-goto-test--is-test-dir))
    ))



(zc-hydra/define zc-ensime-hydra--build
  (:color teal :title "Ensime Build" :prefix "b")
  ("SBT"
   (("bc" ensime-sbt-do-compile "sbt compile")
    ("bC" ensime-sbt-do-clean "sbt clean")
    ("bi" ensime-sbt-switch "sbt switch")
    ("bp" ensime-sbt-do-package "sbt package")
    ("br" ensime-sbt-do-run "sbt run"))))

(zc-hydra/define zc-ensime-hydra--error
  (:color teal :title "Ensime Error" :prefix "e")
  ("Typecheck"
   (("et" ensime-typecheck-current-buffer "typecheck buffer")
    ("eT" ensime-typecheck-all "typecheck all"))

   "Show Error"
   (("ee" ensime-print-errors-at-point "error at point")
    ("el" ensime-show-all-errors-and-warnings "show all errors")
    ("es" ensime-stacktrace-switch "stacktrace switch"))))

(zc-hydra/define zc-ensime-hydra--debug
  (:color teal :title "Ensime Debug" :prefix "d")
  ("Debugger"
   (("dA" ensime-db-attach "db attach")
    ("dc" ensime-db-continue "db continue")
    ("di" ensime-db-inspect-value-at-point "db inspect value")
    ("dn" ensime-db-next "db next")
    ("do" ensime-db-step-out "db step out")
    ("dq" ensime-db-quit "db quit")
    ("dr" ensime-db-run "db run")
    ("ds" ensime-db-step "db step")
    ("dt" ensime-db-backtrace "db backtrace"))

   "Breakpoint"
   (("db" ensime-db-set-break "db set break")
    ("dB" ensime-db-clear-break "db clear break")
    ("dC" ensime-db-clear-all-breaks "db clear all breaks"))))

(zc-hydra/define zc-ensime-hydra--navigation
  (:color teal :title "Ensime Navigation" :prefix "g")
  ("Basic"
   (("gg" ensime-edit-definition "goto definition")
    ("gp" ensime-pop-find-definition-stack "pop definition stack")
    ("gi" ensime-goto-impl "goto impl")
    ("gt" ensime-goto-test "goto test"))))

(zc-hydra/define zc-ensime-hydra--help
  (:color teal :title "Ensime Help & Docs" :prefix "h")
  ("Help & Docs"
   (("hd" ensime-show-doc-for-symbol-at-point "doc at point")
    ("hH" ensime-type-at-point-full-name "type fullname")
    ("hh" ensime-type-at-point "type at point")
    ("hu" ensime-show-uses-of-symbol-at-point "show usage"))))

(zc-hydra/define zc-ensime-hydra--refactor
  (:color teal :title "Ensime Refactor" :prefix "r")
  ("Import"
   (("ri" ensime-import-type-at-point "import type at point")
    ("rI" ensime-inspect-type-at-point-other-frame "inspect type other window")
    ("rp" ensime-inspect-project-package "inspect project package"))

   "Format"
   (("rf" ensime-sbt-do-scalariform-only "sbt format")
    ("ri" ensime-refactor-diff-organize-imports "organize imports"))

   "Misc."
   (("ra" ensime-refactor-add-type-annotation "add type annotation")
    ("rd" ensime-refactor-diff-inline-local "inline local")
    ("rD" ensime-undo-peek "undo peek")
    ("rm" ensime-refactor-diff-extract-method "extract method")
    ("rr" ensime-refactor-diff-rename "rename")
    ("rv" ensime-refactor-diff-extract-local "extract local"))))

(zc-hydra/define zc-ensime-hydra--server
  (:color teal :title "Ensime Server" :prefix "n")
  ("Basic"
   (("nf" ensime-reload "reload")
    ("nF" ensime-reload-open-files "reload open files")
    ("ns" ensime "start server"))

   "Config"
   (("nS" 'zc-ensime/ensime-gen-and-restart "generate and restart"))))

(zc-hydra/define zc-ensime-hydra--test
  (:color teal :title "Ensime Test" :prefix "t")
  ("Basic"
   (("ta" ensime-sbt-do-test-dwim "run test")
    ("tr" ensime-sbt-do-test-quick-dwim "run quick test")
    ("tt" ensime-sbt-do-test-only-dwim "run test only"))))

(zc-hydra/define zc-ensime-hydra--repl
  (:color teal :title "Ensime REPL" :prefix "s")
  ("Send"
   (("sa" ensime-inf-load-file "load file")
    ("sb" ensime-inf-eval-buffer "eval buffer")
    ("sB" zc-ensime/ensime-inf-eval-buffer-switch "eval buffer switch")
    ("si" ensime-inf-switch "switch")
    ("sr" ensime-inf-eval-region "eval region")
    ("sR" zc-ensime/ensime-inf-eval-region-switch "eval region switch"))))

(defmacro zc-ensime/hydra-define (mode)
  (declare (indent defun))
  `(zc-hydra/major-mode-define ,mode
     ("Basic"
      (("/" ensime-search-ivy "search")
       ("'" ensime-inf-switch "switch")
       ("z" ensime-expand-selection-command "expand selection"))

      ""
      (("b" zc-ensime-hydra--build/body "+build")
       ("e" zc-ensime-hydra--error/body "+error")
       ("d" zc-ensime-hydra--debug/body "+debug")
       ("g" zc-ensime-hydra--navigation/body "+navigation")
       ("h" zc-ensime-hydra--help/body "+help"))

      ""
      (("r" zc-ensime-hydra--refactor/body "+refactor")
       ("n" zc-ensime-hydra--server/body "+server")
       ("t" zc-ensime-hydra--test/body "+test")
       ("s" zc-ensime-hydra--repl/body "+repl")))))

(provide 'zc-ensime)
