;;; core/zc-prelude/autoload/buffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zc/copy-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;;;###autoload
(defun zc/buffer-narrow (&optional action)
  "Restrict editing in this buffer to the current region or
org subtree if in `org-mode'.

- If buffer is narrowed, invert the status by `widen'.
- If buffer is narrowed by `consult-focus-lines', invert the status.
- If the region is active, narrow to region.
- If currently in `org-mode', narrow to subtree.
- Otherwise, narrow to defun."
  (interactive)
  (save-excursion
    (cond
     ((or (eq action 'widen)
          (and (buffer-narrowed-p) (eq action 'toggle)))
      (when (buffer-narrowed-p)
        (widen) (recenter))
      (when consult--focus-lines-overlays
        (consult-focus-lines t)))
     ((use-region-p) (narrow-to-region (region-beginning) (region-end)))
     ((eq major-mode 'org-mode) (org-narrow-to-subtree))
     (t                         (narrow-to-defun)))))

;;;###autoload
(defun zc/buffer-clone-indirect ()
  "Create an indirect buffer for the current buffer."
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (let ((org-indirect-buffer-display 'other-window))
      (call-interactively 'org-tree-to-indirect-buffer)))
   (t
    (call-interactively 'clone-indirect-buffer-other-window))))

;; https://www.emacswiki.org/emacs/BufferLocalKeys
(defun zc/buffer-local-set-key (key func)
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
    (funcall (intern name) t)))

(defun zc/buffer-visible-p (buf)
  "Return t if BUF does belongs to any window."
  (get-buffer-window buf))

(defun zc/buffer-invisible-p (buf)
  "Return t if BUF doesn't belongs to any window."
  (not (get-buffer-window buf)))

(defmacro zc/with-widen-buffer (&rest body)
  "Execute body while temporarily widening the buffer."
  (declare (debug (body)))
  `(save-excursion (save-restriction (widen) ,@body)))

(defun zc/assert-buffer-and-file-exists (&optional buffer)
  "Return filename if exists, otherwise throw."
  (let ((fname (buffer-file-name buffer)))
    (if (not (and fname (f-exists? fname)))
        (error "Buffer not visiting a file!")
      fname)))

;;;###autoload
(defun zc/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

;;;###autoload
(defun zc/copy-buffer-name ()
  "Show and copy the name of the current buffer."
  (interactive)
  (if current-prefix-arg
      (funcall #'+default/yank-buffer-path nil)
    (let ((name (-if-let* ((path (buffer-file-name)))
                    (file-name-nondirectory path)
                  (buffer-name))))
      (message (kill-new name)))))

;;;###autoload
(defun zc/copy-buffer-path ()
  "Show and copy the full path to the current buffer."
  (interactive)
  (-if-let* ((path (or (buffer-file-name) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))

;;;###autoload
(defun zc/delete-buffer-and-file (buffer)
  "Removes file associated to the BUFFER and kill the buffer.

Also invalidates projectile cache when it's possible."
  (interactive (list (current-buffer)))
  (let ((fname (buffer-file-name buffer)))
    (if (not (and fname (f-exists? fname)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file fname t)
        (kill-buffer buffer)
        (when (and (featurep 'projectile) (projectile-project-p))
          (projectile-invalidate-cache nil))
        (message "File deleted: %s" fname)))))

;;;###autoload
(defun zc/rename-buffer-and-file (buffer dest-path)
  "Renames BUFFER and the associated file to DEST-PATH.

Also invalidates projectile cache when it's possible and
update recentf list."
  (interactive (let* ((src (zc/assert-buffer-and-file-exists)))
                 (list (current-buffer)
                       (read-file-name "Rename to:"))))
  (let* ((src (zc/assert-buffer-and-file-exists buffer))
         (src-name (f-filename src))
         (dest-dir (f-dirname dest-path))
         (dest-name (f-filename dest-path)))
    (when (and (f-exists? dest-path)
               (not (y-or-n-p "File already exists. Overwrite? ")))
      (user-error "Aborted"))
    ;; Ensure the destination directory exists
    (when (and (not (file-exists-p dest-dir))
               (yes-or-no-p (format "Create directory '%s'?" dest-dir)))
      (make-directory dest-dir t))
    ;; Actually rename the file
    (rename-file src dest-path t)
    ;; Rename current buffer
    (with-current-buffer buffer
      (rename-buffer dest-path)
      (set-visited-file-name dest-path)
      (set-buffer-modified-p nil))
    ;; Update recentf list if needed
    (when (fboundp 'recentf-add-file)
      (recentf-add-file dest-path)
      (recentf-remove-if-non-kept src))
    ;; Update projectile if needed
    (when (and (featurep 'projectile) (projectile-project-p))
      (projectile-invalidate-cache nil))
    (message "File renamed '%s' to '%s'" src-name dest-name)))
