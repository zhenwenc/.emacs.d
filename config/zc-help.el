(eval-when-compile
  (require 'use-package))

(require 'zc-hydra-funcs)



(use-package info
  :general
  ;; Unset evil conflicted keybindings
  (:keymaps 'Info-mode-map "l" nil "h" nil)

  (:states '(normal motion) :keymaps 'Info-mode-map
            "gs" #'Info-goto-node
            "gu" #'Info-follow-reference))

(use-package helpful
  :straight t

  ;; Evil mess up the default keybindings
  :general
  (:states '(normal motion) :keymaps 'helpful-mode-map
           "TAB" #'forward-button
           "n"   #'forward-button
           "p"   #'backward-button)

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
    ;; Prefer reusing the same buffer while navigating to source.
    (advice-add 'helpful--navigate
                :around #'zc-help/temporary-remove-dedication)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helpful ")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.5)))))

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



(zc-hydra/major-mode-define Info-mode
  ("Basic"
   ()

   "Navigation"
   (("gs" Info-goto-node "goto node")
    ("gt" Info-top-node "goto top node")
    ("gT" Info-toc "table of contents")
    ("gu" Info-follow-reference "follow reference")
    ("gm" Info-menu "menu"))

   "Hints"
   (("C-j" Info-next "next")
    ("C-k" Info-prev "previous")
    ("C-o" Info-history-back "history back")
    ("C-i" Info-history-forward "history forward"))))



(provide 'zc-help)
