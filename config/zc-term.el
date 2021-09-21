(eval-when-compile
  (require 'use-package))

(require 'zc-term-funcs)



(use-package term
  :straight nil
  :hydra
  ("Basic"
   (("c" zc-term/open "create")
    ("r" zc-term/rename "rename")
    ("x" zc-term/kill-and-close "kill"))

   "Navigation"
   (("s" zc-term/switch "switch")
    ("<" multi-term-prev "previous" :color red)
    (">" multi-term-next "next" :color red))

   "Toggle"
   (("h" compilation-shell-minor-mode "compilation mode"))))

(use-package multi-term
  :straight t
  :commands (multi-term)
  :general
  (:states 'normal :keymaps 'term-mode-map
   "<return>" #'undefined
   "SPC"      #'zc-main-hydra/body
   "q"        #'zc-term/bury-all-buffers)

  (:states 'insert :keymaps 'term-raw-map
   ;; This key was shallowed by evil-collection,
   ;; but not sure for what purpose.
   "C-y" #'term-paste
   "C-l" #'comint-clear-buffer)

  (:states 'normal :keymaps 'term-mode-map
   :predicate '(zc-term/compilation-minor-mode-p)
   "<return>" #'compile-goto-error
   "RET"      #'compile-goto-error
   "C-n"      #'compilation-next-error
   "C-p"      #'compilation-previous-error)

  :hook (term-mode . zc-term/setup)
  :config
  (defun zc-term/setup ()
    ;; Fix ansi-term bi-directional text support problem, which
    ;; seems to be the cause of text jumbling when going back
    ;; commands in ansi-term.
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
    (setq bidi-paragraph-direction 'left-to-right)

    ;; Final newline isn't needed for comint mode
    (make-local-variable 'require-final-newline)
    (setq require-final-newline nil))

  (setq multi-term-dedicated-window-height 20
        multi-term-switch-after-close 'PREVIOUS
        ;; Avoid interpreter output causes window to scroll
        multi-term-scroll-show-maximum-output t))

(use-package vterm
  :straight t
  :commands (vterm)
  ;; https://github.com/akermu/emacs-libvterm#installation
  :if (and module-file-suffix ; dynamic module
           (executable-find "make")
           (executable-find "cmake")
           (executable-find "libtool"))
  :bind (:map vterm-mode-map
         ([f9] . shell-pop))
  :init (setq vterm-always-compile-module t))

(use-package shell-pop
  :straight t
  :commands (shell-pop)
  :bind ([f9] . shell-pop)
  :init (setq shell-pop-window-size 30
              shell-pop-shell-type
              (cond ((fboundp 'vterm) '("vterm" "*vterm*" #'vterm))
                    (t '("multi-term" "*multi-term*" #'multi-term)))))



(provide 'zc-term)
