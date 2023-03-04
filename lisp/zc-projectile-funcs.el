(require 's)
(require 'f)
(require 'dash)
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

(defun zc-projectile/refresh-cache ()
  "Invalidate projectile caches."
  (interactive)
  (call-interactively 'projectile-invalidate-cache)
  (call-interactively 'zc-projectile/refresh-projects))



(defun zc-projectile/find-project-root (project)
  "Return the best guessed project root directory for the given
projectile project name. Don't use `projectile-project-name'."
  (-find (-partial #'s-ends-with? (concat project "/"))
         projectile-known-projects))

(defun zc-projectile/find-package-root (&optional project)
  "Return the best guessed package root directory for the given
projectile project name. Useful for monorepo."
  (-when-let*
      ((current-directory (or project default-directory))
       (marker-files (-flatten (list (projectile-project-type-attribute 'npm 'marker-files))))
       (package-root (--reduce-from
                      (or acc (projectile-locate-dominating-file current-directory it))
                      nil marker-files)))
    (f-relative package-root (projectile-acquire-root))))

(defun zc-projectile/yarn-workspaces ()
  "Return the list of Yarn workspaces."
  (with-demoted-errors "Error listing yarn workspaces: %S"
    (->> (shell-command-to-string "yarn workspaces --json info")
         (json-read-from-string)
         (alist-get 'data)
         (json-read-from-string)
         (-map (-lambda ((package-name . (&alist 'location location)))
                 (list :name package-name :location location))))))



(defmacro zc-projectile/with-switch-project-action (action &rest body)
  "Execute the forms in BODY while advicing projectile switch
to a project buffer after `projectile-switch-project-by-name'
instead of prompt with `projectile-find-file', which is done
by using the magic dynamic binding."
  (declare (indent defun))
  `(let ((projectile-switch-project-action
          (lambda ()
            ;; Fallback to default switch project action.
            ;; Note that we are on a temporary buffer, see
            ;; `projectile-switch-project-by-name'.
            (let* ((name (projectile-project-name))
                   (root (zc-projectile/find-project-root name))
                   (default-directory root))
              (pcase ,action
                ;; Switch to the previous visited buffer.
                ('previous
                 (projectile-previous-project-buffer))
                ;; Switch to the specified buffer, be careful
                ;; the buffer may not track to a file.
                ((pred ,(-orfn 'bufferp 'stringp))
                 (switch-to-buffer ,action))
                ;; Evaluate a function.
                ((pred functionp)
                 (funcall ,action root name))
                ;; Evaluate a self-evaluation form (sexp).
                ((and (pred listp)
                      (pred (-compose 'functionp 'car)))
                 (eval ,action))
                ;; Fallback to projectile find file.
                (_
                 (consult-projectile-find-file)))))))
     ,@body))



(provide 'zc-projectile-funcs)
