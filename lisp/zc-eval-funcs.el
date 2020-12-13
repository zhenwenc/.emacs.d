(require 's)
(require 'dash)

(autoload 'ivy-read "ivy")
(autoload 'recompile "compile")
(autoload 'compilation-find-buffer "compile")

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
  (ivy-read prompt (delete-dups (mapcar #'s-trim compile-history))
            :history 'zc-eval/projectile-read-command-history
            :initial-input command
            :action #'(lambda (x) (add-to-list 'compile-history x))
            :caller #'zc-eval/projectile-read-command))

;;;###autoload
(defun zc-eval/recompile ()
  "Call `recompile' if the compilation buffer was compiled
manually, otherwise does nothing."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))



(define-minor-mode zc-eval/compile-on-save-mode
  "Minor mode to automatically call `recompile' after saving
the current buffer, when there is no ongoing compilation."
  :lighter " cos"
  :global nil
  (cond
   (noninteractive) ; running a batch job
   ((bound-and-true-p zc-eval/compile-on-save-mode)
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook #'zc-eval/recompile nil t))
   (t
    (remove-hook 'after-save-hook #'zc-eval/recompile t))))



(provide 'zc-eval-funcs)
