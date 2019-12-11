(eval-when-compile
  (require 'use-package))

(require 'pretty-hydra)
(require 'zc-hydra-funcs)



(use-package info
  :general
  ;; Unset evil conflicted keybindings
  (:keymaps 'Info-mode-map "l" nil "h" nil)

  (:states '(normal motion) :keymaps 'Info-mode-map
   "gs" #'Info-goto-node
   "gu" #'Info-follow-reference)

  :hydra
  ((:mode Info-mode)
   ("Navigation"
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

  :config
  (setq info-lookup-other-window-flag nil))

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
             (set-window-dedicated-p window nil)
             (apply orig-fn args)
             (set-window-dedicated-p window t))
            (t
             (apply orig-fn args)))))

  :preface
  (defun zc-help/maybe-kill-buffer (orig-fn &rest args)
    (let ((buffer (current-buffer)))
      ;; Try the original function first
      (apply orig-fn args)
      ;; Kill the helpful buffer if it still alive
      (when (and (derived-mode-p 'helpful-mode)
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (kill-buffer-and-window)))))

  :config
  ;; Prefer reusing the same buffer while navigating to source.
  (advice-add 'helpful--navigate :around #'zc-help/temporary-remove-dedication)
  (advice-add 'helpful--manual   :around #'zc-help/temporary-remove-dedication)
  (advice-add 'helpful--info     :around #'zc-help/temporary-remove-dedication)

  ;; After navigate to any reference then come back to the
  ;; helpful buffer, `quit-window' won't work.
  (advice-add 'quit-window :around #'zc-help/maybe-kill-buffer))

(use-package youdao-dictionary
  :straight t
  :commands (youdao-dictionary-search
             youdao-dictionary-search-from-input))



(provide 'zc-help)
