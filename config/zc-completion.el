(eval-when-compile
  (require 'use-package))

(require 'zc-paths)



;; Vertical completion UI based on the default completion system.
;;
;; https://github.com/minad/vertico
(use-package vertico
  :straight (:files (:defaults
                     "extensions/vertico-directory.el"
                     "extensions/vertico-multiform.el"))
  :general
  (:keymaps 'vertico-map
   "C-j"        #'vertico-next
   "C-k"        #'vertico-previous
   "C-n"        #'vertico-next-group
   "C-p"        #'vertico-previous-group

   "C-."        #'embark-act
   "C-<return>" #'embark-dwim
   "C-h B"      #'embark-bindings
   ;; Export the matched candidates to an `occur-mode'buffer.
   ;; - Press `i' to edit the matches in place with `wgrep'
   "C-c C-e"    #'embark-export)

  :init
  (vertico-mode)
  (setq vertico-count 15) ;; maximum number of candidates to render
  (setq vertico-scroll-margin 0))

;; Provide Ido-like directory navigation commands.
;;
(use-package vertico-directory
  :straight nil
  :after (vertico))

;; Configure Vertico modes per command or completion category.
;;
(use-package vertico-multiform
  :straight nil
  :after (vertico))

;; Display minibuffer in childframe.
;;
(use-package vertico-posframe
  :straight t
  :after (vertico vertico-multiform)
  :if (zc/childframe-workable-p)
  :custom-face
  (vertico-posframe        ((t (:background ,(doom-color 'bg-alt)))))
  (vertico-posframe-border ((t (:background ,(doom-color 'bg-alt)))))
  (vertico-posframe-cursor ((t (:background ,(doom-color 'blue)))))
  :config
  (setq vertico-posframe-border-width 8
        vertico-posframe-parameters '((left-fringe  . 4)
                                      (right-fringe . 4))
        vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)

  ;; If `auto-hscroll-mode' is set to `t', it will scroll horizontally to the
  ;; right margin that causes candidates and prompt to be invisible.
  ;;
  ;; https://www.emacswiki.org/emacs/TruncateLines
  (defun zc/post-vertico-posframe-display (&rest _ignore)
    (when vertico-posframe--buffer
      (with-current-buffer vertico-posframe--buffer
        (setq-local auto-hscroll-mode 'current-line))))
  (advice-add #'vertico-posframe--display :after #'zc/post-vertico-posframe-display)

  (vertico-posframe-mode))

;; TODO Fail to load extension
;;
;; (use-package vertico-repeat
;;   :straight (vertico :includes vertico-repeat
;;                      :files (:defaults "extensions/vertico-repeat.el"))
;;   :after vertico
;;   :ensure nil)

;; Customize completion style, candidates matcher and sorting algorithm
;;
;; https://github.com/oantolin/orderless
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable richer annotations, similar to `ivy-rich'.
;;
;; https://github.com/minad/marginalia
(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))



;; Integrations for the built-in completion function `completing-read'.
;;
;; https://github.com/minad/consult
(use-package consult
  :straight t
  :after embark-consult
  :init
  (setq consult-async-input-debounce 0.1)
  ;; By default, it searches from the current line, which results in a
  ;; very confusing list of candidates, where items in the bottom are
  ;; appearing on the top of the buffer.
  (setq consult-line-start-from-top t)
  :config
  ;; Disable automatic immediate preview on expensive commands
  (consult-customize consult-ripgrep consult-git-grep consult-org-heading
                     :preview-key "C-l")

  ;; Enhance `consult-imenu' command
  (defun zc-completion/consult-imenu (&optional widenp)
    (interactive "p")
    (pcase imenu-create-index-function
      ('org-imenu-get-tree
       (zc-org/outline-buffer-heading (if widenp 'file 'parent)))
      (_
       (call-interactively #'consult-imenu)))))

;; Interactive functions for `ivy', borrow its more mature features
;;
;; - `zc-org/outline-buffer-heading'
;;
(use-package counsel :straight t)



;; Emacs Mini-Buffer Actions Rooted in Keymaps
;;
;; https://github.com/oantolin/embark
(use-package embark
  :straight t
  :after embark-consult
  :init
  ;; Peplace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :demand t ; only necessary when using embark-collect-mode hook
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



(provide 'zc-completion)
