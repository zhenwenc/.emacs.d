(eval-when-compile
  (require 'use-package))

(use-package helpful
  :straight t

  :preface
  (defun zc-help/temporary-remove-dedication (orig-fn &rest args)
    (let ((window (selected-window)))
      (cond ((window-dedicated-p window)
             (set-window-dedicated-p (selected-window) nil)
             (apply orig-fn args)
             (set-window-dedicated-p (selected-window) t))
            (t
             (apply orig-fn args)))))

  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helpful ")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.5)))

    ;; I prefer reusing the same buffer while navigating to source.
    (advice-add 'helpful--navigate
                :around #'zc-help/temporary-remove-dedication)))

(use-package google-translate
  :straight t
  :config
  (setq
   google-translate-default-source-language "en"
   google-translate-default-target-language "zh-CN"
   google-translate-pop-up-buffer-set-focus t)

  (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Google Translate*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.2))))

(provide 'zc-help)
