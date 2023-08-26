;;; tools/zc-projectile/autoload.el -*- lexical-binding: t; -*-

(require 'dash)


;; Helpers

(defun zc/projectile-ignore-projects-filter (dir)
  (let ((-compare-fn 'f-descendant-of?))
    (-contains? zc/projectile-ignored-project-dirs dir)))

(defun zc/projectile-ignored-project-p (project)
  (thread-last zc/projectile-ignored-dirs
               (-map #'file-truename)
               (-any? (-orfn
                       (-rpartial #'f-same? project)
                       (-rpartial #'f-ancestor-of? project)))))

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

(defun zc-projectile/local-workspaces (parent)
  "Return the list of children folders."
  (with-demoted-errors "Error listing dummy workspaces: %S"
    (->> (f-glob parent)
         (--filter (f-directory-p it))
         (--map (f-relative it default-directory)))))


;; Macros

(defmacro zc/projectile-with-switch-project-action (action &rest body)
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


;; Consult backends

(defvar zc/consult-projectile-source-package-file
  (list :name     "Package File"
        :narrow   '(?p . "Package File")
        :category 'file
        :face     'consult-file
        :history  'file-name-history
        :hidden   t ; Hidden by default, unless narrowed
        :action   (lambda (f) (consult--file-action
                               (concat (projectile-acquire-root)
                                       (zc-projectile/find-package-root) f)))
        :enabled  #'projectile-project-root
        :items
        (lambda () (-when-let* ((package-root (zc-projectile/find-package-root)))
                     (->> (projectile-project-files (projectile-acquire-root))
                          (--filter (s-starts-with? package-root it))
                          (--map (s-chop-prefix package-root it)))))))

(defvar zc/consult-projectile-source-workspace-file
  (list :name     "Workspace File"
        :narrow   '(?w . "Workspace File")
        :category 'file
        :face     'consult-file
        :history  'file-name-history
        :action   (lambda (f) (projectile-find-file-in-directory
                               (concat (projectile-acquire-root) f)))
        :enabled  #'projectile-project-root
        :items
        (lambda () (projectile-with-default-dir (projectile-acquire-root)
                     (-concat (->> (zc-projectile/yarn-workspaces)
                                   (--map (plist-get it :location)))
                              (zc-projectile/local-workspaces "./modules/*/*")
                              (zc-projectile/local-workspaces "./examples/*"))))))


;;; Commands

;;;###autoload
(defun zc/projectile-refresh-projects ()
  "Update `projectile-known-projects', append magit discovered
repos, and remove projects if non-exist or is the directory or sub-
directory in `zc/projectile-ignored-dirs'."
  (interactive)
  (when (require 'magit nil t)
    ;; Add all projects that detected by magit
    (mapc #'projectile-add-known-project
          (mapcar (-compose #'file-name-as-directory #'f-abbrev)
                  (magit-list-repos)))
    ;; Filter the known projects
    (thread-last projectile-known-projects
                 (-remove (-compose
                           #'zc/projectile-ignored-project-p
                           #'file-truename))
                 (setq projectile-known-projects))
    ;; Ensure the projects exist
    (projectile-cleanup-known-projects)))

;;;###autoload
(defun zc/projectile-refresh-cache ()
  "Invalidate projectile caches."
  (interactive)
  (require 'projectile)
  (call-interactively 'projectile-invalidate-cache)
  (call-interactively 'zc/projectile-refresh-projects))

;;;###autoload
(defun zc/consult-projectile-find-file ()
  "Enhanced `consult-projectile-find-file' with extra sources"
  (interactive)
  (funcall-interactively #'consult-projectile
                         '(consult-projectile--source-projectile-file
                           zc/consult-projectile-source-package-file)))

;;;###autoload
(defun zc/consult-projectile-find-file-in-dir ()
  "Enhanced `consult-projectile-find-file' with extra sources"
  (interactive)
  (funcall-interactively #'consult-projectile
                         '(zc/consult-projectile-source-workspace-file)))
