(require 'f)
(require 's)
(require 'evil)
(require 'subr-x)
(require 'zc-paths)

(autoload 'iedit-quit "iedit")
(autoload 'evil-escape "evil")
(autoload 'org-move-item-up "org")
(autoload 'org-move-item-down "org")
(autoload 'recentf-remove-if-non-kept "recentf")
(autoload 'projectile-project-p "projectile")
(autoload 'projectile-project-name "projectile")
(autoload 'projectile-invalidate-cache "projectile")
(autoload 'projectile-previous-project-buffer "projectile")
(autoload 'counsel-projectile-switch-project "counsel-projectile")


;; Buffer

(defun zc/copy-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun zc/buffer-toggle-narrow (beg end)
  "Restrict editing in this buffer to the current region or
org subtree if in `org-mode'.

- If buffer is narrowed, cancel the narrowing by `widen'.
- If the region is active, narrow to region.
- If currently in `org-mode', narrow to subtree.
- Otherwise, narrow to defun."
  (interactive "r")
  (save-excursion
    (cond
     ((buffer-narrowed-p)        (widen) (recenter))
     ((use-region-p)             (narrow-to-region beg end))
     ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
     (t                          (narrow-to-defun)))))


;; Window

(defun zc/toggle-maximize-window ()
  "Maximize window"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun zc/split-window-below-and-focus (no-focus)
  "Split the window vertically and focus the new window.

With prefix arg NO-FOCUS, don't select the new window."
  (interactive "P")
  (split-window-below)
  (unless no-focus (windmove-down)))

(defun zc/split-window-right-and-focus (no-focus)
  "Split the window horizontally and focus the new window.

With prefix arg NO-FOCUS, don't select the new window."
  (interactive "P")
  (split-window-right)
  (unless no-focus (windmove-right)))

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

(defun zc/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun zc/copy-buffer-name ()
  "Show and copy the name of the current buffer."
  (interactive)
  (let ((name (-if-let* ((path (buffer-file-name)))
                  (file-name-nondirectory path)
                (buffer-name))))
    (message (kill-new name))))

(defun zc/copy-buffer-path ()
  "Show and copy the full path to the current buffer."
  (interactive)
  (-if-let* ((path (or (buffer-file-name) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))

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
          (call-interactively #'projectile-invalidate-cache))
        (message "File deleted: %s" fname)))))

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
    (when (projectile-project-p)
      (call-interactively #'projectile-invalidate-cache))
    (message "File renamed '%s' to '%s'" src-name dest-name)))


;; Editing

(defun zc-core/backward-kill-line (arg)
  "Kill ARG lines backward.

Behave the same as 'Command + delete' at macOS"
  (interactive "p")
  (kill-line (- 1 arg)))

(defun zc-core/move-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun zc-core/move-line-down ()
  "Move the current line down."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(defun zc-core/evil-escape-and-save ()
  "Evil escape everything and save buffer."
  (interactive)
  (zc-core/evil-escape)
  (save-buffer))

;; Evil escape everything
(defun zc-core/evil-escape ()
  "Evil escape everything."
  (interactive)
  (if (bound-and-true-p iedit-mode)
      (iedit-quit))
  ;; (evil-mc-undo-all-cursors)
  (evil-escape))


;; Symbol and Search

(defun zc/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (cl-case evil-search-module
    ;; NOTE: We no longer use persist highlights
    ;; ('isearch (evil-search-highlight-persist-remove-all))
    ('evil-search (evil-ex-nohighlight))))


;; Misc.

(defun zc/kill-emacs-or-frame (&optional persist-server-p)
  "Kill emacs process or the current frame."
  (interactive)
  (if persist-server-p
      (condition-case-unless-debug nil
          (delete-frame nil 1)
        (error
         (make-frame-invisible nil 1)))
    (kill-emacs)))

(defun zc/load-private-package (pkg file)
  "Load encrypted package PKG from private directory."
  (let ((path (f-join paths-private-directory file)))
    (if (f-exists? path)
        (require pkg path)
      (warn "Private package [%s] not found." path))))



(provide 'zc-funcs)
