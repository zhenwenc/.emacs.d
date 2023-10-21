;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; For macOS, remap the following Modifier Keys for external keyboards:
;;
;; - Caps Lock (⇧) key  => ^ Control
;; - Control (^) key    => [ Escape
;; - Option (⌥) key     => ⌘ Command
;; - Command (⌘) key    => ⌥ Option
;; - Function (fn) key  => ⌥ Option
;;

;; Disable evil-snipe on s/S keys
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(setq evil-snipe-override-evil-repeat-keys nil)

;; Default editor settings
(setq-default tab-width 2)
(setq-default js-indent-level 2)

;; https://github.com/json-emacs/json-mode#indent-width
(setq-hook! 'json-mode-hook js-indent-level 2)

;; Doom default to 'loopback, which forces gpg-agent to use the Emacs minibuffer
;; to prompt for the key passphrase. It doesn't use macos keychain.
;; FIXME What's the better approach?
(after! epa
  (set epg-pinentry-mode nil))

;; Prepare initial workspace layouts on startup.
(add-hook! 'after-init-hook
  (defun zc/init-workspace-layouts-hooks-h ()
    (zc/projectile-refresh-projects)
    (zc-layout/init-workspaces '(("~/.doom.d"  "init.el")
                                 ("~/.emacs.d" ".gitignore")
                                 ("~/dotfiles" ".gitignore")))))

;; Default maximize frame and enter fullscreen mode
(add-hook! 'after-init-hook
  (defun zc/init-emacs-frame ()
    ;; NOTE Some Emacs release doesn't respect frame parameter
    ;; (add-hook 'emacs-startup-hook #'toggle-frame-maximized)
    ;; (run-at-time "2sec" nil (lambda () (toggle-frame-fullscreen)))
    (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
    ;; Customize popup rules
    (set-popup-rule! "^\\*compilation" :size 0.5 :side 'right :ttl nil :select t)))

;; Wrap long lines in the buffer with language-aware indentation by
;; - `adaptive-wrap'
;; - `visual-line-mode'
;; - `visual-fill-column-mode'
(add-hook! 'after-init-hook
  (defun zc/init-editor-config ()
    (when (doom-module-p :editor word-wrap)
      (setq +word-wrap-extra-indent nil) ;; weird experience
      (+global-word-wrap-mode +1))))
