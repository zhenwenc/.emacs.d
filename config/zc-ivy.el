(eval-when-compile
  (require 'use-package))

(require 's)
(require 'noflet)



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

  :preface
  (progn
    (defun zc-ivy/show-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun zc-ivy/occur-then-wgrep ()
      "Shortcut for calling `ivy-occur' then activating wgrep."
      (interactive)
      (ivy-occur)
      (ivy-wgrep-change-to-wgrep-mode))

    (defun zc-ivy/sort-matches-by-length (_name candidates)
      "Re-sort CANDIDATES, prioritize shorter length."
      (cl-sort (copy-sequence candidates)
               (lambda (x y)
                 (let ((tx (s-trim x))
                       (ty (s-trim y)))
                   (or (< (length tx) (length ty))
                       (string< tx ty)))))))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")

    ;; Fuzzy matching result sorting
    ;; http://oremacs.com/2016/01/06/ivy-flx/
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))

    ;; Increase the maximum number of candidates that will be sorted
    ;; using `flx'. The default is 200, which means `flx' is almost
    ;; never used. Setting it too high (e.g. 10000) causes lag.
    (setq ivy-flx-limit 2000)

    ;; Keep minibuffer even if no input
    (set-variable 'ivy-on-del-error-function '(lambda()))

    (dolist (item '((helpful-key      . "^")
                    (helpful-callable . "^")
                    (helpful-variable . "^")
                    (helpful-macro    . "^")))
      (add-to-list 'ivy-initial-inputs-alist item))

    ;; Re-sort matching candicates
    (add-to-list 'ivy-sort-matches-functions-alist
                 '(counsel-projectile-find-file . zc-ivy/sort-matches-by-length))

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
    (setq counsel-find-file-ignore-regexp
          (regexp-opt '("node_modules")))))

(use-package flx
  :straight t
  :after ivy)

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
