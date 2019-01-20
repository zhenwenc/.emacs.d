(eval-when-compile
  (require 'use-package))

(require 's)
(require 'noflet)
(require 'zc-ivy-funcs)



(use-package ivy
  :straight t

  :general
  (:keymaps 'ivy-occur-mode-map
            "C-x C-w"    #'ivy-wgrep-change-to-wgrep-mode)

  (:keymaps 'ivy-minibuffer-map
            "<f1>"       #'zc-ivy/show-help
            "C-j"        #'ivy-next-line
            "C-k"        #'ivy-previous-line
            "C-w"        #'ivy-yank-word  ; match isearch behaviour
            "C-l"        #'ivy-partial-or-done
            "C-z"        #'ivy-dispatching-done
            "C-<return>" #'ivy-immediate-done
            "C-c C-e"    #'zc-ivy/occur-then-wgrep)

  :config
  (progn
    (setq ivy-count-format "(%d/%d) ")

    ;; Show recent files in switch-buffer
    (setq ivy-use-virtual-buffers t)

    ;; Keep minibuffer even if no input
    (setq ivy-on-del-error-function nil)

    (dolist (item '((helpful-key      . "^")
                    (helpful-callable . "^")
                    (helpful-variable . "^")
                    (helpful-macro    . "^")))
      (add-to-list 'ivy-initial-inputs-alist item))

    ;; Re-sort matching candicates
    (add-to-list 'ivy-sort-matches-functions-alist
                 '(counsel-projectile-find-file . zc-ivy/sort-matches-by-length))

    ;; Use ivy for yasnippet prompt
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-prompt-functions #'zc-ivy/yas-prompt nil #'eq))

    (ivy-mode)))

(use-package ivy-hydra
  :straight t)

(use-package counsel
  :straight t
  :general
  (:states '(motion normal insert visual)
           "M-x" #'counsel-M-x
           "M-y" #'counsel-yank-pop)
  :config
  (progn
    ;; Display separator in kill-ring buffer
    (setq counsel-yank-pop-separator
          (propertize (concat "\n" (make-string 30 ?-) "\n")
                      'face '(:foreground "gray50")))

    ;; Ignore noisy files and directories
    (setq counsel-find-file-ignore-regexp (regexp-opt '("node_modules")))

    ;; The default counsel rg command ending with a dot, this will
    ;; produce duplicated result for `counsel-projectile-rg'.
    (setq counsel-rg-base-command "rg -zS -M 120 --no-heading --line-number --color never %s ."
          counsel-ag-base-command "ag -zS -W 120 --nocolor --nogroup %s")))

(use-package flx
  :straight t
  :after ivy
  :config
  (setq
   ;; Fuzzy matching result sorting
   ;; http://oremacs.com/2016/01/06/ivy-flx/
   ivy-re-builders-alist
   '((t . ivy--regex-plus))

   ;; Increase the maximum number of candidates that will be sorted
   ;; using `flx'. The default is 200, which means `flx' is almost
   ;; never used. Setting it too high (e.g. 10000) causes lag.
   ivy-flx-limit 2000))

(use-package historian
  :straight t
  :commands (historian-mode)
  :after ivy
  :config (historian-mode +1))

(use-package ivy-historian
  :straight t
  :commands (ivy-historian-mode)
  :after (:and ivy historian)
  :config
  (progn
    (setq ivy-historian-freq-boost-factor 2000)
    (setq ivy-historian-recent-boost 2000)
    (setq ivy-historian-recent-decrement 1000)

    (ivy-historian-mode 1)))

(provide 'zc-ivy)
