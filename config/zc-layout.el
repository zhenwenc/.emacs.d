(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'zc-layout-funcs)



(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-tagged-slot-format "%s [%t]")

  ;; Unset all default key bindings
  (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)

  (eyebrowse-mode +1)

  ;; Prepare initial layers on startup.
  (-each-r
   '(("~/.emacs.d" ".gitignore")
     ("~/dotfiles" ".gitignore"))
   (-lambda ((project buffer))
     (message "Before creating layout %s" buffer)
     (zc-projectile/with-switch-project-action
       '(find-file buffer) ; open initial buffer
       (zc-layout/create-project-layout project)
       (setq-local default-directory project))))

  ;; Switch to scratch buffer
  (let ((dir default-directory))
    (switch-to-buffer "*scratch*" t)
    (setq default-directory dir)))

(use-package winum
  :straight t
  :general
  ("M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4
   "M-5" #'winum-select-window-5)
  :config
  (setq winum-auto-assign-0-to-minibuffer nil
        winum-auto-setup-mode-line nil
        winum-ignored-buffers '(" *which-key*")
        winum-scope 'frame-local)
  (winum-mode))

(use-package winner
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  (winner-mode t))

(use-package ace-window
  :straight t
  :defer t
  :commands ace-window
  :general ("M-o" 'ace-window)
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?8)
        aw-background t))


;; Popup Windows

(dolist (item `((,(rx bos "*tide-documentation*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-width    . 0.5))

                (,(rx bos "*prodigy*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible))

                (,(rx bos "*explain-pause-top*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 0.3))

                (,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.2))

                (,(rx bos "*SQL*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.5))

                (,(rx bos "*Org Agenda*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.5))

                (,(rx bos "*Org Src")
                 (display-buffer-reuse-window
                  display-buffer-same-window)
                 (reusable-frames . visible)
                 (slot            . 1))

                (,(rx bos "*eshell" (*? anything) "*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.35))

                (,(rx bos "*helpful ")
                 (display-buffer-reuse-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.5))

                (,(rx bos "*wclock*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.2))

                (,(rx bos "*Go Translate*" eos)
                 (display-buffer-reuse-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.3))

                (,(rx bos "*compilation*" eos)
                 (display-buffer-reuse-window)
                 (reusable-frames . visible)
                 (slot            . 1)
                 (window-height   . 0.5))

                (,(rx bos "*prettier errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.3))

                (,(rx bos "*rustfmt*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 1)
                 (window-height   . 0.2))

                (,(rx bos "*lsp-diagnostics*" eos)
                 (display-buffer-reuse-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.5))

                (,(rx bos "*PLANTUML Preview*" eos)
                 (display-buffer-reuse-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.5))
                ))
  (add-to-list 'display-buffer-alist item))



(provide 'zc-layout)
