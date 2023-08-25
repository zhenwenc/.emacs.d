(eval-when-compile
  (require 'use-package))



(setq mac-command-modifier    'meta
      mac-option-modifier     'super
      mac-command-key-is-meta t
      mac-option-key-is-meta  nil

      ;; In dired, move deletions to trash
      delete-by-moving-to-trash t

      ;; Emacs child frame won't work on native fullscreen mode.
      ;; see lazycat-emacs
      ns-use-native-fullscreen nil
      ns-use-fullscreen-animation nil

      ;; Don't open a file in a new frame
      ns-pop-up-frames nil
      ;; Never show a proxy icon in the title bar.
      ns-use-proxy-icon nil

      ;; Enable this when using maximized instead of fullframe.
      ns-auto-hide-menu-bar nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance           . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars    . nil))

(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-p"))

(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'evil-yank)
(global-set-key (kbd "M-z") 'undo-tree-undo)
(global-set-key (kbd "M-Z") 'undo-tree-redo)
(global-set-key (kbd "M-s") 'zc-core/evil-escape-and-save)
(global-set-key (kbd "M-Z") 'undo-tree-redo)
(global-set-key (kbd "<C-M-268632070>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-C-SPC") 'ns-do-show-character-palette)



(defun zc-osx/finder (&optional file)
  "Opens file directory in Finder."
  (interactive (list (buffer-file-name)))
  (if file
      (shell-command
       (format "%s %s" (executable-find "open") (file-name-directory file)))
    (error "Buffer is not attached to any file")))



(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize))

(use-package osx-trash
  :straight t
  :if (memq window-system '(mac ns x))
  :preface (autoload 'osx-trash-setup "osx-trash")
  :config (osx-trash-setup))

(provide 'zc-osx)
