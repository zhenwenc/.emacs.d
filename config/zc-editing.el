(eval-when-compile
  (require 'use-package))

(require 'zc-paths)



(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "s-<left>")      'backward-word)
(global-set-key (kbd "s-<right>")     'forward-word)

(global-set-key (kbd "M-<backspace>") 'zc-core/backward-kill-line)
(global-set-key (kbd "C-<left>")      'backward-sentence)
(global-set-key (kbd "C-<right>")     'forward-sentence)
(global-set-key (kbd "C-<return>")    'zc-core/start-newline-next)

(global-set-key (kbd "M-<left>")      'mwim-beginning-of-code-or-line)
(global-set-key (kbd "M-<right>")     'mwim-end-of-code-or-line)

(global-set-key (kbd "TAB")           'indent-for-tab-command)
(global-set-key (kbd "C-i")           'indent-for-tab-command)

;; I-Search
(define-key isearch-mode-map (kbd "M-v")           'isearch-yank-pop)
(define-key isearch-mode-map (kbd "M-<backspace>") 'isearch-delete-char)



(use-package whitespace
  :preface
  (defun zc-editing/show-trailing-whitespace ()
    (setq show-trailing-whitespace 1))

  :preface
  (defun zc-editing/set-whitespace-style-for-diff ()
    (setq-local whitespace-style
                '(face
                  trailing
                  tabs tab-mark
                  spaces space-mark
                  newline newline-mark
                  indentation::space
                  indentation::tab)))

  :hook ((prog-mode . zc-editing/show-trailing-whitespace)
         (diff-mode . zc-editing/set-whitespace-style-for-diff)

         ;; Cleanup whitespace on save
         (before-save . whitespace-cleanup)))

;; FIXME This package is required for `evil-iedit-state', why?
;; https://github.com/syl20bnr/evil-iedit-state/blob/master/evil-iedit-state.el#L60
(use-package auto-highlight-symbol
  :straight t
  :after evil
  :preface
  ;; Prevent the default keymap from getting created
  (defvar auto-highlight-symbol-mode-map (make-sparse-keymap))
  :config
  (require 'zc-evil-ahs)

  (setq ahs-case-fold-search nil)
  (setq ahs-default-range 'ahs-range-whole-buffer)
  (setq ahs-idle-interval 0.25)
  (setq ahs-inhibit-face-list nil)

  ;; Disable by default, use keybinding instead.
  ;; See `zc-evil-ahs'
  (setq ahs-idle-timer 0))

(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp . aggressive-indent-mode)))

(use-package expand-region
  :straight t
  :config
  ;; HACK Disable `org-mode' extensions due to performance issue
  (defun zc/er-save-org-mode-excursion (action) (funcall action))
  (advice-add #'er/save-org-mode-excursion :override #'zc/er-save-org-mode-excursion)
  (advice-add #'er/mark-org-element        :override #'ignore)
  (advice-add #'er/mark-org-element-parent :override #'ignore)
  (advice-add #'er/mark-org-code-block     :override #'ignore)
  (advice-add #'er/mark-org-parent         :override #'ignore)

  ;; HACK These expand functions have problem with YAML variable placeholders
  (advice-add #'er/mark-yaml-inner-block   :override #'ignore)
  (advice-add #'er/mark-yaml-outer-block   :override #'ignore))

(use-package hippie-exp
  :general ("M-/" 'hippie-expand
            :states 'insert
            [remap evil-complete-previous] 'hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev               ; searching the current buffer
          try-expand-dabbrev-all-buffers   ; searching all other buffers
          try-expand-dabbrev-from-kill     ; searching the kill ring
          try-complete-file-name-partially ; complete text as a file name
          try-complete-file-name           ; complete text as a file name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package dumb-jump
  :straight t
  :general (:keymaps '(emacs-lisp-mode-map)
            [remap xref-find-definitions] #'dumb-jump-go
            [remap xref-pop-marker-stack] #'dumb-jump-back)
  :config
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-prefer-searcher 'rg)))

(use-package iedit
  :straight t
  :general (:keymaps 'iedit-mode-keymap
            "C-;" #'iedit-toggle-selection)
  :config
  (progn
    (setq iedit-toggle-key-default nil)
    (set-face-background 'iedit-occurrence "PaleVioletRed4")))

(use-package wgrep
  :straight t
  :config (setq wgrep-auto-save-buffer t))


;; Visual

;; Line wraps at `fill-column' instead of window edge. Mainly used
;; while editing Org-Mode or Markdown files.
;;
;; https://www.philnewton.net/blog/distraction-free-writing-with-emacs
(use-package visual-fill-column
  :straight t
  :commands (visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 100)
  ;; Patch the `split-window-preferred-function' function to
  ;; remove added window margin before splitting, otherwise
  ;; vertical window split may not behave as expected.
  (setq visual-fill-column-enable-sensible-window-split t))



(provide 'zc-editing)
