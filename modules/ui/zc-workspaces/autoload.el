(require 'dash)


;;; Helpers

(defun zc-layout/slot-unoccupied-p (slot)
  "Return t if SLOT is yet unoccupied."
  (let* ((windows (eyebrowse--get 'window-configs))
         (window  (assoc slot windows))
         (tag     (nth 2 window)))
    (or (not window) ; window was deleted
        (= (length tag) 0))))

(defun zc-layout/find-free-slot ()
  "Returns a yet unoccupied eyebrowse slot. Takeover the default
slot if unoccupied, otherwise fine a free one."
  (if (zc-layout/slot-unoccupied-p eyebrowse-default-workspace-slot)
      eyebrowse-default-workspace-slot
    (let* ((window-configs (eyebrowse--get 'window-configs))
           (slots          (mapcar 'car window-configs)))
      (eyebrowse-free-slot slots))))

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

(defun zc-layout/config-for-slot (&optional slot)
  "Return the window config plist for SLOT.

If SLOT is nil, default to current slot."
  (ht-get zc-layout/window-config-alist
          (or slot (eyebrowse--get 'current-slot))))

(defun zc-layout/project-for-slot (&optional slot)
  "Return the associated project for SLOT.

See also `zc-layout/config-for-slot'."
  (plist-get (zc-layout/config-for-slot slot) :project))

(defun zc-layout/layout-tag-for-project (dir)
  "Return the eyebrowse window config tag for PROJECT."
  (--> projectile-known-projects
       (--filter (f-same? dir it) it)
       (f-common-parent it)
       (s-chop-prefix (f-full it) (f-full dir))
       (s-chop-suffix "/" it)))

(defun zc-layout/is-current-project-layout-p ()
  "Return t if the current selected project is the same as
the layout project."
  (let* ((current (projectile-project-root))
         (project (zc-layout/project-for-slot)))
    (and project (string= project current))))

(defun zc-layout/refresh-current-window-config ()
  (setq zc-layout/current-window-config-tag
        (zc-layout/current-window-config-tag)))
(dolist (hook '(eyebrowse-post-window-switch-hook
                projectile-after-switch-project-hook
                projectile-before-switch-project-hook))
  (add-hook hook #'zc-layout/refresh-current-window-config))


;;; Commands

;;;###autoload
(defun zc-layout/select-project-no-action ()
  "Prompt project selection with counsel."
  (completing-read (projectile-prepend-project-name "Select a project: ")
                   projectile-known-projects nil t))

;;;###autoload
(defun zc-layout/create-project-layout (&optional dir)
  "Return the layout slot for the project in DIR, or create a new
layout if no layout found and return the created slot.

REMARK: You can control the projectile action after switching to a
project by `projectile-switch-project-action' variable. For example,
wrap your action within `zc/projectile-with-switch-project-action'.
"
  (interactive)
  (require 'map)
  (let* ((dir     (or dir (zc-layout/select-project-no-action)))
         (project (projectile-project-root dir))
         (tag     (zc-layout/layout-tag-for-project project)))
    (-if-let (found (zc-layout/find-window-config-for-tag tag))
        (eyebrowse-switch-to-window-config (car found))
      ;; Create eyebrowse window config
      (let ((slot (zc-layout/find-free-slot)))
        (eyebrowse-switch-to-window-config slot)
        ;; Rename the window config tag
        (eyebrowse-rename-window-config slot tag)
        ;; Actually switch to the project
        (projectile-switch-project-by-name project)
        ;; Memorize the window config associated project
        (map-put! zc-layout/window-config-alist slot
                  (list :project project :tag tag))
        ;; Kill other windows, they belong to the last layout
        (delete-other-windows)))))

;;;###autoload
(defun zc-layout/switch-project-layout (&optional create)
  "Switch to a layout for the project in DIR, create a new layout
if not found or NEW."
  (interactive "P")
  (if (and zc-layout/window-config-alist (not create))
      (eyebrowse-switch-to-window-config (eyebrowse--read-slot))
    (zc/projectile-with-switch-project-action 'default
      (zc-layout/create-project-layout))))

;;;###autoload
(defun zc-layout/kill-buffer (arg &optional buffer)
  "Kill the current buffer, switch to the previous project buffer.

If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (let* ((buffer (or buffer (current-buffer)))
         (window (get-buffer-window buffer)))
    (cond ((window-minibuffer-p)
           (abort-recursive-edit))
          ((equal '(4) arg)
           (kill-buffer-and-window))
          (t
           (zc-layout/switch-to-previous-buffer window)
           (kill-buffer buffer)))))

;;;###autoload
(defun zc-layout/switch-to-previous-buffer (&optional win)
  "Switch to the previous project buffer if available, otherwise
switch to fallback buffer."
  (interactive)
  (with-selected-window (or win (selected-window))
    (let* ((current (projectile-project-root))
           (project (zc-layout/project-for-slot)))
      ;; When the current project mismatches with the layout's
      ;; project, `projectile-previous-project-buffer' will make
      ;; a wrong choice.
      (cond ((and project (string= project current))
             (projectile-previous-project-buffer))
            (project
             (zc/projectile-with-switch-project-action 'previous
               (projectile-switch-project-by-name project)))
            (t
             (projectile-previous-project-buffer))))))

;;;###autoload
(defun zc-layout/init-workspaces (projects)
  "Initialise workspace layout for projects."
  (-each-r projects (-lambda ((project buffer))
                      (message "Before creating layout %s" project)
                      (zc/projectile-with-switch-project-action `(find-file ,buffer)
                        (zc-layout/create-project-layout project)
                        (setq-local default-directory project)))))
