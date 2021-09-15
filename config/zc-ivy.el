(eval-when-compile
  (require 'use-package))

(require 's)
(require 'zc-ivy-funcs)
(require 'zc-layout-funcs)



(use-package ivy
  :straight t

  :general
  (:keymaps 'ivy-occur-mode-map
   "C-x C-w"    #'ivy-wgrep-change-to-wgrep-mode)

  (:keymaps 'ivy-switch-buffer-map
   "C-k"        #'ivy-previous-line) ;; Don't kill my buffers!

  (:keymaps 'ivy-minibuffer-map
   "<f1>"       #'zc-ivy/show-help
   "C-j"        #'ivy-next-line
   "C-k"        #'ivy-previous-line
   "C-w"        #'ivy-yank-word  ; match isearch behaviour
   "C-l"        #'ivy-partial-or-done
   "C-z"        #'ivy-dispatching-done
   "C-<return>" #'ivy-immediate-done
   "C-c C-e"    #'zc-ivy/occur-then-wgrep)

  (:keymaps 'ivy-reverse-i-search-map
   "C-j"        #'ivy-next-line
   "C-k"        #'ivy-previous-line
   "C-S-k"      #'ivy-reverse-i-search-kill)

  :config
  (setq ivy-height 12)
  (setq ivy-count-format "(%d/%d) ")

  ;; Show recent files in switch-buffer
  (setq ivy-use-virtual-buffers t)

  ;; Keep minibuffer even if no input
  (setq ivy-on-del-error-function nil)

  (dolist (item '((helpful-key               . "^")
                  (helpful-callable          . "^")
                  (helpful-variable          . "^")
                  (helpful-macro             . "^")
                  (counsel-faces             . "^")
                  (counsel-describe-function . "^")
                  (counsel-describe-variable . "^")))
    (add-to-list 'ivy-initial-inputs-alist item))

  ;; Re-sort matching candidates
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-projectile-find-file . zc-ivy/sort-matches-by-length))

  ;; Use ivy for yasnippet prompt
  (with-eval-after-load 'yasnippet
    (add-to-list 'yas-prompt-functions #'zc-ivy/yas-prompt nil #'eq))

  (ivy-mode +1))

(use-package ivy-hydra
  :straight t)

(use-package ivy-posframe
  :straight t
  :if (display-graphic-p)
  :custom-face
  (ivy-posframe        ((t (:background ,(doom-color 'bg-alt)))))
  (ivy-posframe-border ((t (:background ,(doom-color 'bg-alt)))))
  (ivy-posframe-cursor ((t (:background ,(doom-color 'blue)))))
  :config
  (setq ivy-posframe-min-height 10
        ivy-posframe-border-width 8
        ivy-posframe-parameters '((left-fringe  . 4)
                                  (right-fringe . 4))
        ivy-posframe-style 'frame-bottom-center-custom)

  (defun zc/posframe-poshandler-frame-center-near-bottom (info)
    (let ((parent-frame (plist-get info :parent-frame))
          (pos (posframe-poshandler-frame-center info)))
      (cons (car pos)
            (truncate (* (frame-pixel-height parent-frame) .618)))))
  (defun ivy-posframe-display-at-frame-bottom-center-custom (str)
    (ivy-posframe--display str #'zc/posframe-poshandler-frame-center-near-bottom))

  (defun zc/ivy-posframe-use-fixed-width ()
    (let ((height (max (or ivy-posframe-height ivy-height) 10))
          (width (min (or ivy-posframe-width 200) (round (* .618 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (setq ivy-posframe-size-function #'zc/ivy-posframe-use-fixed-width)

  (ivy-posframe-mode +1))



(use-package counsel
  :straight t
  :general
  (:states '(motion normal insert visual)
   "M-x" #'counsel-M-x
   "M-y" #'counsel-yank-pop)
  (:keymaps 'swiper-map
   "M-%" #'swiper-query-replace)
  :config
  ;; Display separator in kill-ring buffer
  (setq counsel-yank-pop-separator
        (propertize (concat "\n" (make-string 30 ?-) "\n")
                    'face '(:foreground "gray50")))

  ;; Ignore noisy files and directories
  (setq counsel-find-file-ignore-regexp
        (regexp-opt (append (mapcar (lambda (dir) (s-append "/" dir))
                                    projectile-globally-ignored-directories)
                            projectile-globally-ignored-files)))

  ;; The default counsel rg command ending with a dot, this will
  ;; produce duplicated result for `counsel-projectile-rg'.
  (setq counsel-grep-base-command "rg -S -M 120 --no-heading --line-number --color never %s %s")

  ;; Use `helpful' describe functions
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

;; counsel-projectile also loads projectile itself
(use-package counsel-projectile
  :straight t
  :after (:and counsel projectile)
  :commands (counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-dir)

  :general (:keymaps 'projectile-command-map
            "/"  #'counsel-projectile-rg)

  :config
  (setq counsel-projectile-remove-current-buffer t)
  (setq counsel-projectile-remove-current-project t)

  ;; sort project file candidates with `ivy-prescient'.
  (setq counsel-projectile-sort-files t)

  (counsel-projectile-mode +1))

(use-package counsel-tramp
  :straight t
  :commands (counsel-tramp))



(use-package flx
  :straight t
  :after ivy
  :config
  (setq
   ;; Fuzzy matching result sorting
   ;; http://oremacs.com/2016/01/06/ivy-flx/
   ivy-re-builders-alist '((t . ivy--regex-plus))

   ;; Increase the maximum number of candidates that will be sorted
   ;; using `flx'. The default is 200, which means `flx' is almost
   ;; never used. Setting it too high (e.g. 10000) causes lag.
   ivy-flx-limit 2000))

(use-package ivy-prescient
  :straight t
  :after ivy
  :hook (ivy-mode . ivy-prescient-mode))



(use-package ivy-rich
  :disabled t ;; wip
  :straight t
  :if (display-graphic-p)
  :after (:and ivy projectile)
  :config
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)
  ;; Use abbreviate in `ivy-rich-mode'.
  (setq ivy-virtual-abbreviate
        (or (and ivy-rich-mode 'abbreviate) 'name))
  (ivy-rich-mode +1))

(use-package all-the-icons-ivy-rich
  :disabled t ;; wip
  :straight t
  :if (display-graphic-p)
  :after (:and ivy ivy-rich counsel-projectile)
  :config
  (all-the-icons-ivy-rich-reload))



(provide 'zc-ivy)
