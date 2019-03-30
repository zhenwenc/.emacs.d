(require 's)
(require 'dash)

(autoload 'recompile "compile")
(autoload 'ivy-read "ivy")

(defvar compile-history)
(defvar compilation-arguments)



;;;###autoload
(defun zc-eval/compilation-toggle-comint ()
  "Restart compilation with (or without) `comint-mode'."
  (interactive)
  (cl-callf (lambda (mode) (if (eq mode t) nil t))
      (elt compilation-arguments 1))
  (recompile))

;;;###autoload
(defun zc-eval/compilation-send-input (input &optional nl)
  "Send INPUT to the current process without `comint-mode'.
Interactively also sends a terminating newline."
  (interactive "MInput: \nd")
  (let ((string (concat input (if nl "\n"))))
    ;; for visual feedback.
    (let ((inhibit-read-only t))
      (insert-before-markers string))
    (process-send-string
     (get-buffer-process (current-buffer)) string)))

;;;###autoload
(defun zc-eval/compilation-send-self ()
  "Send the pressed key to the current process."
  (interactive)
  (zc-eval/compilation-send-input
   (apply #'string (append (this-command-keys-vector) nil))))

(defun zc-eval/projectile-read-command (prompt command)
  "Overwrite the original `projectile-read-command', prompt
with Ivy."
  (ivy-read prompt (delete-dups compile-history)
            :history 'compile-history
            :initial-input command
            :caller #'zc-eval/projectile-read-command))



(provide 'zc-eval-funcs)
