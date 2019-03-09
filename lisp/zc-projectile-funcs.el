(require 's)
(require 'f)
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

(defun zc-projectile/find-project-root (project)
  "Return the best guessed project root directory for the given
projectile project name. Don't use `projectile-project-name'."
  (-find (-partial #'s-ends-with? (concat project "/"))
         projectile-known-projects))



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
                ('previous
                 (projectile-previous-project-buffer))
                ((pred ,(-orfn 'bufferp 'stringp))
                 (switch-to-buffer ,action))
                ((pred functionp)
                 (funcall ,action root name))
                (_
                 (projectile-find-file)))))))
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
