(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'subr-x)

(autoload 'projectile-project-p "projectile")
(autoload 'projectile-process-current-project-buffers "projectile")



(use-package flycheck
  :straight t
  :after (hydra)
  :hook (prog-mode . flycheck-mode-on-safe)

  :commands (flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)

  :preface
  (defun zc-flycheck/maybe-inhibit-flycheck (result)
    (unless (equal (buffer-name) "*ediff-merge*") result))

  :init
  (defun zc-flycheck/disable-checkers (&rest checkers)
    "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
    (unless (boundp 'flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers nil))
    (make-local-variable 'flycheck-disabled-checkers)
    (dolist (checker checkers)
      (cl-pushnew checker flycheck-disabled-checkers)))

  :config
  (progn
    (setq flycheck-pos-tip-timeout 10)
    (setq flycheck-display-errors-delay 0.5)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-display-errors-function 'zc-flycheck/display-error-messages)

    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

    (defun zc-flycheck/toggle-error-list ()
      "Show or hide the error list."
      (interactive)
      (-if-let* ((window (--first (equal flycheck-error-list-buffer
                                         (buffer-name (window-buffer it)))
                                  (window-list))))
          (delete-window window)
        (flycheck-list-errors)))

    (defun zc-flycheck/display-error-messages (errors)
      (unless (flycheck-get-error-list-window 'current-frame)
        (when (and errors (flycheck-may-use-echo-area-p))
          (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
            (display-message-or-buffer (string-join messages "\n\n")
                                       flycheck-error-message-buffer
                                       'display-buffer-popup-window)))))

    (advice-add #'flycheck-may-enable-mode :filter-return
                #'zc-flycheck/maybe-inhibit-flycheck)

    (defhydra zc-flycheck-hydra
      (:hint nil :foreign-keys warn
             :pre (progn (setq hydra-lv t) (flycheck-list-errors))
             :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*")))
      "Errors"
      ("f"   flycheck-error-list-set-filter                            "filter")
      ("n"   flycheck-next-error                                       "next")
      ("p"   flycheck-previous-error                                   "previous")
      ("<"   flycheck-first-error                                      "first")
      (">"   (progn (goto-char (point-max)) (flycheck-previous-error)) "last")
      ("RET" flycheck-error-list-goto-error                            "goto")
      ("q"  nil))))



(use-package flycheck-pos-tip
  :straight t
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode))



(provide 'zc-flycheck)
