(eval-when-compile
  (require 'use-package))



(use-package flyspell
  :if (executable-find "aspell")

  :preface
  (defun zc-flyspell/setup ()
    (dolist (key '("C-;" "C-," "C-."))
      (unbind-key key flyspell-mode-map)))

  :hook
  ((org-mode         . flyspell-mode)
   (git-commit-mode  . flyspell-mode)
   (before-save-hook . flyspell-buffer)
   (flyspell-mode    . zc-flyspell/setup))

  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))

  :init
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package flyspell-correct-ivy
  :straight t
  :if (eq system-type 'darwin)
  :general (:keymaps 'flyspell-mode-map
            "C-M-;" #'flyspell-correct-at-point)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))



(provide 'zc-flyspell)
