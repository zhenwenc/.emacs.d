(eval-when-compile
  (require 'use-package))



(use-package flyspell
  :if (executable-find "aspell")

  :preface
  (defun zc-flyspell/shut-up (oldfun &rest args)
    "Quiet down messages in adviced OLDFUN."
    (let ((message-off (make-symbol "message-off")))
      (unwind-protect
          (progn
            (advice-add #'message :around #'ignore (list 'name message-off))
            (apply oldfun args))
        (advice-remove #'message message-off))))

  :preface
  (defun zc-flyspell/setup ()
    (dolist (key '("C-;" "C-," "C-."))
      (unbind-key key flyspell-mode-map)))

  :hook
  ((org-mode         . flyspell-mode)
   (yaml-mode        . flyspell-mode)
   (git-commit-mode  . flyspell-mode)
   (prog-mode        . flyspell-prog-mode)
   (before-save-hook . flyspell-buffer)
   (flyspell-mode    . zc-flyspell/setup))

  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))

  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))

  :config
  (advice-add #'ispell-init-process :around #'zc-flyspell/shut-up))

(use-package flyspell-correct-ivy
  :straight t
  :if (eq system-type 'darwin)
  :bind ("C-M-;" . flyspell-correct-at-point)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))



(provide 'zc-flyspell)
