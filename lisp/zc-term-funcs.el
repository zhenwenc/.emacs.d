(require 'f)
(require 'dash)
(require 'term)

(defvar multi-term-buffer-name)
(defvar multi-term-buffer-list)



(defun zc-term/compilation-minor-mode-p ()
  "Return t if compilation minor mode is enabled."
  (or (bound-and-true-p compilation-shell-minor-mode)
      (bound-and-true-p compilation-minor-mode)))


;; Commands

;;;###autoload
(defun zc-term/open (arg &optional command)
  "Switch to the term buffer last used, or create a new one
with `multi-term' if none exists, or if the current buffer is
already a term."
  (interactive "P")
  (let ((multi-term-program (or command nil))
        (current-prefix-arg nil))
    (if (or arg (derived-mode-p 'term-mode))
        (multi-term)
      (multi-term-prev))))

;;;###autoload
(defun zc-term/kill-and-close ()
  "Kill the current term buffer and process."
  (interactive)
  (unless (derived-mode-p 'term-mode)
    (user-error "Not in a term buffer!"))
  (-when-let (proc (get-buffer-process (current-buffer)))
    (set-process-query-on-exit-flag proc nil)
    (kill-process proc)
    (message "Killed process %s" proc))
  (kill-current-buffer))

;;;###autoload
(defun zc-term/switch ()
  "Switch to a term buffer."
  (interactive)
  (unless multi-term-buffer-list
    (user-error "No `multi-term' buffers exist."))
  (unless (> (length multi-term-buffer-list) 1)
    (user-error "This is the only `multi-term' buffer."))
  (let ((buffers (->> multi-term-buffer-list
                      (-remove-item (current-buffer))
                      (-map 'buffer-name))))
    (ivy-read "Switch to: " buffers
              :action #'switch-to-buffer
              :caller #'zc-term/switch
              :require-match t)))

;;;###autoload
(defun zc-term/bury-all-buffers ()
  "Bury all term buffers."
  (interactive)
  (dolist (buf multi-term-buffer-list)
    (call-interactively 'bury-buffer buf))
  (unless (zc-layout/is-current-project-layout-p)
    (zc-layout/switch-to-previous-buffer)))

;;;###autoload
(defun zc-term/rename (name)
  "Rename the current terminal buffer to NAME."
  (interactive "sRename to: ")
  (unless (derived-mode-p 'term-mode)
    (user-error "Not in a term buffer!"))
  (rename-buffer (format "*%s<%s>*" multi-term-buffer-name name) t))



(provide 'zc-term-funcs)
