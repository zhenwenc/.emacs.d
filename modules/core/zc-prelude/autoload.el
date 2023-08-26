(require 'f)

(autoload 'evil-escape "evil")
(autoload 'evil-ex-nohighlight "evil")
(autoload 'evil-window-set-height "evil")
(autoload 'org-move-item-up "org")
(autoload 'org-move-item-down "org")
(autoload 'org-narrow-to-subtree "org")
(autoload 'recentf-remove-if-non-kept "recentf")
(autoload 'projectile-project-p "projectile")
(autoload 'projectile-project-name "projectile")
(autoload 'projectile-invalidate-cache "projectile")
(autoload 'projectile-previous-project-buffer "projectile")


;; Buffer

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


;; Window

;;;###autoload
(defun zc/toggle-maximize-window ()
  "Maximize window."
  (interactive)
  (let* ((win (window-normalize-window nil))
         (win-height (window-parameter win 'window-height)))
    (cond
     ;; If window is side window, which can not be the only
     ;; window, resize the window
     ((window-parameter win 'window-side)
      (evil-window-set-height nil))
     ;; If window maybe maximized, to restore the previous
     ;; window layout
     ((and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_))
     ;; Miximize the selected window
     (t
      (window-configuration-to-register ?_)
      (delete-other-windows win)))))

;;;###autoload
(defun zc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))


;; File

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


;; Editing

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


;; Symbol and Search

;;;###autoload
(defun zc/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (cl-case evil-search-module
    ;; NOTE: We no longer use persist highlights
    ;; ('isearch (evil-search-highlight-persist-remove-all))
    ('evil-search (evil-ex-nohighlight))))


;; UI

(defun zc/childframe-workable-p ()
  "Return `t' when childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))


;; Secret

(defun zc/load-private-package (pkg file)
  "Load encrypted package PKG from private directory."
  (let ((path (f-join paths-private-dir file)))
    (if (f-exists? path)
        (require pkg path)
      (warn "Private package [%s] not found." path))))

(cl-defun zc/secrets-basic-auth (&rest spec &allow-other-keys)
  (when-let ((found (car (apply 'auth-source-search :require '(:user :secret) spec)))
             (username (plist-get found :user))
             (password (funcall (plist-get found :secret))))
    (format "%s:%s" username password)))


;; Misc.

;;;###autoload
(defun zc/kill-emacs-or-frame (&optional persist-server-p)
  "Kill emacs process or the current frame."
  (interactive)
  (if persist-server-p
      (condition-case-unless-debug nil
          (delete-frame nil 1)
        (error
         (make-frame-invisible nil 1)))
    (kill-emacs)))

(defmacro zc/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))
