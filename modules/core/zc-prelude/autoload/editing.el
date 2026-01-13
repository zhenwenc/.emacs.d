;;; core/zc-prelude/autoload/editing.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zc-core/backward-kill-line (arg)
  "Kill ARG lines backward.

Behave the same as 'Command + delete' at macOS"
  (interactive "p")
  (kill-line (- 1 arg)))

;;;###autoload
(defun zc-core/move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

;;;###autoload
(defun zc-core/move-line-down ()
  "Move the current line down."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

;;;###autoload
(defun zc-core/evil-escape-and-save ()
  "Evil escape everything and save buffer."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (message "You won't want to save!")
    (save-buffer))
  (call-interactively 'zc-core/evil-escape))

;;;###autoload
(defun zc-core/evil-escape ()
  "Evil nuclear escape everything. See also `doom/escape'"
  (interactive)
  (if (bound-and-true-p iedit-mode)
      (iedit--quit))
  (call-interactively 'evil-escape)
  (call-interactively 'doom/escape))

;;;###autoload
(defun zc/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (when (derived-mode-p 'yaml-mode)
    (user-error "You won't wanna indent YAML buffer!"))
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun zc/kill-transform-function (str)
  "Transform STR before putting it on the kill ring.
See `kill-transform-function'"
  (and (not (string-blank-p str))
       str))

;;;###autoload
(defun zc/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (cl-case evil-search-module
    ;; NOTE: We no longer use persist highlights
    ;; ('isearch (evil-search-highlight-persist-remove-all))
    (evil-search (evil-ex-nohighlight))))
