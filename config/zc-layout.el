(eval-when-compile
  (require 'use-package))

(require 's)
(require 'f)
(require 'dash)
(require 'dash-functional)
(require 'counsel-projectile)

(autoload 'projectile-switch-project "projectile")

(defvar zc-layout/window-config-project-alist nil
  "Alist of window config slots with their associated project.

Each element looks like (SLOT . PROJECT).")



(use-package eyebrowse
  :straight t
  :config
  (progn
    (setq eyebrowse-tagged-slot-format "%s [%t]")

    ;; Unset all default key bindings
    (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)

    (eyebrowse-mode t)))

(use-package winum
  :straight t
  :general
  ("M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4
   "M-5" #'winum-select-window-5)
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-ignored-buffers '(" *which-key*"))
    (winum-mode)))



(defun zc-layout/select-project-no-action ()
  "Prompt project selection with counsel."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Select a project: ")
            projectile-known-projects
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :require-match t
            :sort counsel-projectile-sort-projects
            :caller 'zc-layout/select-project))

(defun zc-layout/default-slot-occupied-p ()
  "Return t if the default slot is not yet occupied."
  (let* ((slot eyebrowse-default-workspace-slot)
         (windows (eyebrowse--get 'window-configs))
         (window (assoc slot windows))
         (tag (nth 2 window)))
    (or (not window) ; window was deleted
        (and tag (> (length tag) 0)))))

(defun zc-layout/current-window-config-tag ()
  "Return the current window config tag."
  (-when-let* ((slot (eyebrowse--get 'current-slot))
               (windows (eyebrowse--get 'window-configs))
               (window (assoc slot windows)))
    (-last-item window)))

(defun zc-layout/find-window-config-for-tag (tag)
  "Return t if window configs WINDOWS contains entry with TAG."
  (let ((windows (eyebrowse--get 'window-configs))
        (tag-equals? (-compose (-partial 's-equals? tag) '-last-item)))
    (-find tag-equals? windows)))

(defun zc-layout/switch-to-window-config (slot &optional project)
  "Switch to the window config SLOT. If PROJECT is specified and
mismatch with the current project name, then switch to the project."
  (eyebrowse-switch-to-window-config slot)
  (unless (or (not project)
              (equal project (projectile-project-name)))
    (projectile-switch-project-by-name project)))

(defun zc-layout/get-project-for-slot (&optional slot)
  "Return the associated project for SLOT.

If SLOT is nil, default to current slot."
  (alist-get (or slot (eyebrowse--get 'current-slot))
             zc-layout/window-config-project-alist nil nil 'equal))

(defun zc-layout/has-same-project-name (project)
  "Return true if there exists another project with the same
name as PROJECT in the `projectile-known-projects'."
  (thread-last projectile-known-projects
    (-remove (-partial 'f-same? project))
    (-map #'f-base)
    (-any?  (-partial 's-equals? (f-base project)))))

(defun zc-layout/get-layout-tag-for-project (project)
  "Return the eyebrowse window config tag for PROJECT."
  (let* ((dirname (f-base project))
         (parent  (f-base (f-parent project))))
    (if (zc-layout/has-same-project-name project)
        (format "%s/%s" parent dirname)
      dirname)))

(defun zc-layout/create-project-layout (&rest _)
  "Create a project layout.

- if there is a layout associate with the selected project,
  switch to the layout;
- otherwise, create occupy the next free layout slot, and use
  the project's name as the tag of the window config."
  (interactive)
  (-when-let* ((project (zc-layout/select-project-no-action))
               (tag (zc-layout/get-layout-tag-for-project project)))
    (let ((window (zc-layout/find-window-config-for-tag tag)))
      (if window
          (zc-layout/switch-to-window-config (car window) project)
        ;; Create eyebrowse window config, prefer to the default slot if
        ;; not occupied, otherwise fallback to `eyebrowse-create-window-config'.
        (if (zc-layout/default-slot-occupied-p)
            (eyebrowse-create-window-config)
          (eyebrowse-switch-to-window-config eyebrowse-default-workspace-slot))
        (let ((slot (eyebrowse--get 'current-slot)))
          ;; Rename the window config tag
          (eyebrowse-rename-window-config slot tag)
          ;; Actually switch to the project
          (projectile-switch-project-by-name project)
          ;; Memorize the window config associated project
          (map-put zc-layout/window-config-project-alist
                   slot (projectile-project-name)))
        ;; Kill other windows since they belong to the last layout
        (delete-other-windows)))))

(defun zc-layout/switch-project-layout (&rest _)
  "Switch to a project. If default slot is not occupied,
prompt to create a project layout."
  (interactive)
  (if (zc-layout/default-slot-occupied-p)
      (zc-layout/switch-to-window-config (eyebrowse--read-slot))
    (zc-layout/create-project-layout)))

(defun zc-layout/kill-current-buffer (&optional arg)
  "Kill the current buffer, switch to the previous project buffer.

If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (let* ((buffer       (current-buffer))
         (buffer-win   (get-buffer-window buffer))
         (project      (zc-layout/get-project-for-slot))
         (project-curr (projectile-project-name)))
    (cond ((window-minibuffer-p)
           (abort-recursive-edit))
          ((equal '(4) arg)
           (kill-buffer-and-window))
          (t
           (with-selected-window buffer-win
             ;; When the current project mismatches with the layout's
             ;; project, `projectile-previous-project-buffer' will make
             ;; the situation even worse.
             (if (and project (equal project project-curr))
                 (projectile-previous-project-buffer)
               (previous-buffer))
             (kill-buffer buffer))))))



(provide 'zc-layout)
