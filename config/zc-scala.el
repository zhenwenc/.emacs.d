(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'straight)
(require 'zc-paths)

(defconst zc-metals-executable (concat paths-vendor-dir "metals/metals-emacs"))

(defvar zc-scala/use-unicode-arrows t
  "If non-nil, replace arrows with unicode characters.")


;; Automatically replace arrows with unicode ones when enabled

(defconst zc-scala/unicode-arrows-alist
  '(("=>" . "⇒")
    ("->" . "→")
    ("<-" . "←")))

(defun zc-scala/replace-arrow-at-point ()
  "Replace the arrow at point (if any) with unicode ones.
An undo boundary is inserted before doing the replacement so that
it can be undone."
  (when zc-scala/use-unicode-arrows
    (let* ((end (point))
           (start (max (- end 2) (point-min)))
           (x (buffer-substring start end))
           (arrow (assoc x zc-scala/unicode-arrows-alist)))
      (when arrow
        (undo-boundary)
        (backward-delete-char 2)
        (insert (cdr arrow))))))

(defun zc-scala/unicode-gt ()
  "Insert a `>' to the buffer. If it's part of an right arrow (`->' or `=>'),
replace it with the corresponding unicode arrow."
  (interactive)
  (insert ">")
  (zc-scala/replace-arrow-at-point))

(defun zc-scala/unicode-hyphen ()
  "Insert a `-' to the buffer. If it's part of an left arrow (`<-'),
replace it with the unicode arrow."
  (interactive)
  (insert "-")
  (zc-scala/replace-arrow-at-point))



(use-package scala-mode
  :straight t
  :defer t

  :mode "\\.s\\(cala\\|bt\\|c\\)$"

  :general
  ;; Replace arrows with unicode ones when enabled
  (:keymaps 'scala-mode-map
   ">" #'zc-scala/unicode-gt
   "-" #'zc-scala/unicode-hyphen)

  :preface
  (defun zc-scala/setup ()
    (zc-scala/metals-setup)
    (lsp-deferred))

  :preface
  (defun zc-scala/metals-setup (&optional forced)
    "Download Metals Scala Language Server for Emacs."
    (interactive "P")
    (unless (and (not forced)
                 (executable-find zc-metals-executable))
      (unless (executable-find "coursier")
        (error "Unable to install metals! Required 'coursier' executable."))
      (message "Downloading Metals Scala Language Server...")
      ;; https://scalameta.org/metals/docs/editors/emacs.html
      ;; https://scalameta.org/metals/docs/editors/new-editor.html
      (shell-command (concat "coursier bootstrap"
                             " --java-opt -Xss4m"
                             " --java-opt -Xms100m"
                             " --java-opt -XX:+UseStringDeduplication"
                             " --java-opt -Dmetals.client=emacs"
                             " --java-opt -Dmetals.verbose=off"
                             " org.scalameta:metals_2.12:0.7.0"
                             " -r bintray:scalacenter/releases"
                             " -r sonatype:snapshots"
                             " -o " zc-metals-executable
                             " -f"))
      (message "Downloaded Metals Scala Language Server!")))

  :preface
  (defun zc-scala/disable-flycheck-scala ()
    "Disable scala checker if ensime mode is active"
    (when (boundp 'flycheck-disabled-checkers)
      (push 'scala flycheck-disabled-checkers)))

  :hook ((scala-mode  . zc-scala/setup)
         (scala-mode  . flycheck-mode-on-safe)
         (ensime-mode . zc-scala/disable-flycheck-scala))

  :init
  ;; Configure LSP client
  (setq lsp-metals-server-command zc-metals-executable)
  ;; For Play Framework
  (add-to-list 'auto-mode-alist '("/conf/routes\\'" . conf-mode))
  ;; Hope it will be faster
  (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
    (add-to-list 'completion-ignored-extensions ext))

  :config
  (setq scala-indent:align-forms t)
  (setq scala-indent:align-parameters t)
  (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)

  (setq flycheck-scalastylerc "~/.scalastyle.xml")

  ;; Compatibility with `aggressive-indent'
  (setq scala-indent:align-forms t
        scala-indent:align-parameters nil
        scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)

  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sbt-file-mode))

  ;; (with-eval-after-load 'ensime
  ;;   (setq ensime-sem-high-faces
  ;;         `((var                . scala-font-lock:var-face)
  ;;           (varField           . scala-font-lock:var-face)
  ;;           (functionCall       . font-lock-function-name-face)
  ;;           (operator           . font-lock-keyword-face)
  ;;           (param              . (:slant italic))
  ;;           (class              . font-lock-type-face)
  ;;           (trait              . (:inherit font-lock-type-face :slant italic))
  ;;           (object             . font-lock-constant-face)
  ;;           (package            . font-lock-preprocessor-face)
  ;;           (implicitConversion . (:underline ,(with-no-warnings "#2aa198")))
  ;;           (implicitParams     . (:underline ,(with-no-warnings "#2aa198")))
  ;;           (deprecated         . (:strike-through "dark gray")))))
  )

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))



(provide 'zc-scala)
