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
  (progn
    (defun zc-editing/show-trailing-whitespace ()
      (setq show-trailing-whitespace 1))

    (defun zc-editing/set-whitespace-style-for-diff ()
      (setq-local whitespace-style
                  '(face
                    trailing
                    tabs tab-mark
                    spaces space-mark
                    newline newline-mark
                    indentation::space
                    indentation::tab))))

  :hook ((prog-mode . zc-editing/show-trailing-whitespace)
         (diff-mode . zc-editing/set-whitespace-style-for-diff)

         ;; Cleanup whitespace on save
         (before-save . whitespace-cleanup)))

(use-package auto-highlight-symbol
  :straight t
  :after evil
  :preface
  ;; Prevent the default keymap from getting created
  (defvar auto-highlight-symbol-mode-map (make-sparse-keymap))
  :config
  (progn
    (require 'zc-evil-ahs)

    (setq ahs-case-fold-search nil)
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 0.25)
    (setq ahs-inhibit-face-list nil)

    ;; Disable by default, use keybinding instead.
    ;; See `zc-evil-ahs'
    (setq ahs-idle-timer 0)))

(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp . aggressive-indent-mode)))

(use-package expand-region
  :straight t)

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
  :general (:states 'normal :keymaps 'prog-mode-map
                    "M-." #'dumb-jump-go
                    "M-," #'dumb-jump-back)
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

(use-package savehist
  :config
  (setq savehist-file (concat paths-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(compile-history))
  (savehist-mode +1))



(provide 'zc-editing)
