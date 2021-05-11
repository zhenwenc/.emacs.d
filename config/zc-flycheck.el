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
  ;; Reduce recheck frequency and display error quicker
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-display-errors-delay 0.5)

  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (defun zc-flycheck/toggle-error-list ()
    "Show or hide the error list."
    (interactive)
    (-if-let* ((window (--first (equal flycheck-error-list-buffer
                                       (buffer-name (window-buffer it)))
                                (window-list))))
        (delete-window window)
      (flycheck-list-errors)))

  (defun zc-flycheck/maybe-inhibit-flycheck (result)
    (unless (equal (buffer-name) "*ediff-merge*") result))
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
    ("q"  nil)))


;; Display Flycheck errors

(use-package flycheck-posframe
  :straight t
  :after (flycheck)
  :if (display-graphic-p)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom-face
  (flycheck-posframe-info-face ((t (:foreground ,(doom-color 'green)))))
  :config
  ;; Don't display popups if company is open
  (with-eval-after-load 'company
    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))
  ;; Don't display popups while in insert or replace mode, as it can affect
  ;; the cursor's position or cause disruptive input delays.
  (with-eval-after-load 'evil
    (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
    (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p)))

(use-package flycheck-pos-tip
  :disabled ; use posframe
  :straight t
  :after (flycheck)
  :if (display-graphic-p)
  :config
  (setq flycheck-pos-tip-timeout 10)
  (flycheck-pos-tip-mode))



(provide 'zc-flycheck)
