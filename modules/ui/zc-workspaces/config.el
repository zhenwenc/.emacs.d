;;; ui/zc-workspaces/config.el -*- lexical-binding: t; -*-

(defvar zc-layout/window-config-alist (make-hash-table)
  "Alist of layout configurations keyed by eyebrowse window
config slot.

Each element looks like (SLOT . PLIST), where

:project
The associated projectile project root directory.

:tag
The eyebrowse window config tag, doesn't guarantee uniqueness.")

(defvar zc-layout/current-window-config-tag nil
  "The cached current window config tag. This is useful to
reduce the overhead of recomputing the layout info.")



(use-package! eyebrowse
  :unless noninteractive
  :config
  (setq eyebrowse-tagged-slot-format "%s [%t]")
  ;; Unset all default key bindings
  (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)
  ;; Init eyebrowse mode
  (eyebrowse-mode +1))
