(require 's)
(require 'dash)
(require 'dash-functional)
(require 'subr-x)
(require 'general)

(defvar projectile-known-projects)

(defconst zc-projectile/ignored-dirs
  '("~/.emacs.d/straight/"))



(defun zc-projectile/ignored-project-p (project)
  (thread-last zc-projectile/ignored-dirs
    (-map #'file-truename)
    (-any? (-orfn
            (-rpartial #'f-same? project)
            (-rpartial #'f-ancestor-of? project)))))

(defun zc-projectile/refresh-projects ()
  "Update `projectile-known-projects', append magit discovered
repos, and remove projects if non-exist or is the directory or sub-
directory in `zc-projectile/ignored-dirs'."
  (interactive)
  (when (require 'magit nil t)
    ;; Add all projects that detected by magit
    (mapc #'projectile-add-known-project
          (mapcar (-compose #'file-name-as-directory #'f-abbrev)
                  (magit-list-repos)))
    ;; Filter the known projects
    (thread-last projectile-known-projects
      (-remove (-compose
                #'zc-projectile/ignored-project-p
                #'file-truename))
      (setq projectile-known-projects))
    ;; Ensure the projects exist
    (projectile-cleanup-known-projects)))



(defmacro zc-projectile/with-switch-project-action (to-buffer &rest body)
  "Execute the forms in BODY while advicing projectile switch
to a project buffer after `projectile-switch-project-by-name'
instead of prompt with `projectile-find-file', which is done
by using the magic dynamic binding."
  (declare (indent defun))
  `(let ((projectile-switch-project-action
          (lambda ()
            (let ((buf (cond
                        (,to-buffer ,to-buffer)
                        (t "*scratch*"))))
              (message "Switched to buffer: %s" buf)
              (switch-to-buffer buf)))))
     ,@body))


;; Commands

;;;###autoload
(defun zc-projectile/search-symbol-at-point (current-dir-p)
  (interactive "P")
  (let ((sym (thing-at-point 'symbol t)))
    (if (and (projectile-project-p) (not current-dir-p))
        (let ((counsel-projectile-rg-initial-input sym))
          (counsel-projectile-rg))
      (counsel-rg sym default-directory ""
                  (format "[%s] rg" default-directory)))))



(provide 'zc-projectile-funcs)
