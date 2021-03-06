(eval-when-compile
  (require 'use-package))

(require 'general)



(use-package evil
  :straight t

  :defines (evil-want-Y-yank-to-eol)
  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)

  :general
  ;; Motion keys for help buffers.
  (:states 'motion :keymaps 'help-mode-map
   "<escape>"   #'quit-window
   "<tab>"      #'forward-button
   "S-<tab>"    #'backward-button
   "]"          #'help-go-forward
   "["          #'help-go-back
   "gh"         #'help-follow-symbol)

  ;; Rebind C-u to scroll up
  (:states '(motion normal visual)
   "j"          #'evil-next-visual-line
   "k"          #'evil-previous-visual-line
   "C-u"        #'evil-scroll-up
   "C-d"        #'evil-scroll-down
   "M-u"        #'evil-scroll-line-up
   "M-d"        #'evil-scroll-line-down
   "C-i"        #'indent-for-tab-command
   "C-o"        #'goto-last-change
   "C-S-o"      #'goto-last-change-reverse
   "M-a"        #'zc-core/evil-escape
   "M-s"        #'zc-core/evil-escape-and-save
   "M-C-<up>"   #'zc-core/move-line-up
   "M-C-<down>" #'zc-core/move-line-down)

  (:states '(normal insert)
   "M-k"        #'kill-whole-line
   "M-."        #'xref-find-definitions
   "M-,"        #'xref-pop-marker-stack)

  (:states 'normal
   "q"          nil ; unset define macro
   "M"          #'counsel-evil-marks)

  (:states 'insert
   "C-<tab>"    #'dabbrev-expand
   "TAB"        #'indent-for-tab-command
   "M-a"        #'zc-core/evil-escape
   "M-s"        #'zc-core/evil-escape-and-save
   "C-k"        #'undefined ; digraph
   "C-i"        #'indent-for-tab-command
   "C-d"        #'delete-char
   "C-S-d"      #'backward-kill-word)

  (:states '(normal visual) "C-e" #'evil-end-of-line)
  (:states 'insert          "C-e" #'mwim-end-of-code-or-line)

  :init
  (setq evil-want-integration t
        evil-want-Y-yank-to-eol t
        evil-want-keybinding nil ; required for evil-collection
        ;; FIXME: This may lead to unwanted behaviour, such as
        ;;        when cursor on the closing curly braces.
        evil-want-visual-char-semi-exclusive t
        evil-mode-line-format nil
        evil-insert-skip-empty-lines t)

  :config
  (setq evil-shift-width 2)
  (setq evil-symbol-word-search t)

  ;; Configure cursors.
  (setq evil-motion-state-cursor '("plum3" box))
  (setq evil-visual-state-cursor '("gray" (hbar . 2)))
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
  (setq evil-emacs-state-cursor  '("SkyBlue2" hbar))

  ;; Configure initial state for modes
  (evil-set-initial-state 'diff-mode                'motion)
  (evil-set-initial-state 'ibuffer-mode             'motion)
  (evil-set-initial-state 'prodigy-mode             'motion)
  (evil-set-initial-state 'process-menu-mode        'motion)
  (evil-set-initial-state 'org-agenda-mode          'motion)
  (evil-set-initial-state 'ivy-occur-mode           'motion)
  (evil-set-initial-state 'compilation-mode         'motion)
  (evil-set-initial-state 'tide-references-mode     'motion)
  (evil-set-initial-state 'tide-project-errors-mode 'normal)

  (evil-mode +1))


;; Enhance text objects, motion and seeking behavior.

(use-package evil-args ;; for delimited arguments
  :straight t
  :after evil
  :config
  (require 'evil-args)
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))


;; Provide integration with highlight-symbol

(use-package zc-evil-ahs
  :after (:and evil pretty-hydra))


;; A set of keybindings for Evil mode.

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))


;; Define key sequence to escape from insert state

(use-package evil-escape
  :straight t
  :after evil
  :init
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2
                evil-escape-unordered-key-sequence nil
                evil-escape-excluded-states '(motion normal visual)
                evil-escape-excluded-major-modes
                '(help-mode treemacs-mode ibuffer-mode image-mode
                            magit-mode
                            magit-diff-mode
                            magit-cherry-mode
                            magit-process-mode
                            magit-log-mode
                            magit-refs-mode
                            magit-status-mode))
  :config
  (evil-escape-mode))


;; Evil states to interface iedit mode

(use-package evil-iedit-state
  :straight t
  :commands (evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil))


;; Edit surroundings in pairs

(use-package evil-surround
  :straight t
  :commands (global-evil-surround-mode)
  :general
  (:states 'visual :keymaps 'evil-surround-mode-map
   "s" #'evil-surround-region
   "S" #'evil-substitute)
  :preface
  (defun zc-evil/init-evil-surround-pairs ()
    (make-local-variable 'evil-surround-pairs-alist)
    (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))
  :hook
  ;; TODO: Why need this hook?
  (emacs-lisp-mode . zc-evil/init-evil-surround-pairs)
  :init
  (setq-default evil-surround-pairs-alist
                '((?\( . ("(" . ")"))
                  (?\[ . ("[" . "]"))
                  (?\{ . ("{" . "}"))

                  (?\) . ("(" . ")"))
                  (?\] . ("[" . "]"))
                  (?\} . ("{" . "}"))

                  (?# . ("#{" . "}"))
                  (?b . ("(" . ")"))
                  (?B . ("{" . "}"))
                  (?> . ("<" . ">"))
                  (?$ . ("${" . "}"))
                  (?t . evil-surround-read-tag)
                  (?< . evil-surround-read-tag)
                  (?f . evil-surround-function)))
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode +1)))


;; Extend % matching for HTML, LaTex, etc.

(use-package evil-matchit
  :straight t
  :init
  (with-eval-after-load 'evil
    (global-evil-matchit-mode +1)))


;; Comment/uncomment lines efficiently

(use-package evil-nerd-commenter
  :straight t
  :after evil-common
  :general
  (:states 'normal
   ;; Double all the commenting functions so that the inverse
   ;; operations can be called without setting a flag
   ";"  #'evilnc-comment-operator
   "gc" #'evilnc-comment-operator))


;; Supplemental evil-mode key-bindings to org-mode

(use-package evil-org
  :straight t
  :after (:and org evil-common)
  :hook (org-mode . evil-org-mode)

  :general
  (:states 'normal :keymaps 'org-mode-map "t" #'org-todo)

  :init
  ;; Disable overriding `C-t' and `C-d' keybindings.
  (setq evil-disable-insert-state-bindings t)

  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  ;; Remove weird keybindings.
  (general-unbind :states '(normal insert) :keymaps 'evil-org-mode-map
    "M-l" "M-h" "J" "O" "M-l" "M-h"))



(provide 'zc-evil)
