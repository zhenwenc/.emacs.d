(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'subr-x)
(require 'seq)
(require 'f)



(defconst paths-cache-dir
  (concat user-emacs-directory ".cache/"))

(defconst paths-lisp-dir
  (concat user-emacs-directory "lisp/"))

(defconst paths-config-dir
  (concat user-emacs-directory "config/"))

(defconst paths-vendor-dir
  (concat user-emacs-directory "vendor/"))

(defconst paths-themes-dir
  (concat user-emacs-directory "themes/"))

(defconst paths-private-dir
  (concat "~/dotfiles/" "private/"))



(defun zc-paths/init-load-paths (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs
          (list paths-lisp-dir paths-config-dir))
         (subdirs
          (f-directories paths-lisp-dir))
         (updated-load-path
          (seq-filter #'file-directory-p
                      (seq-uniq (append main-dirs subdirs load-path)))))

    (setq load-path updated-load-path)
    (add-to-list 'custom-theme-load-path paths-themes-dir)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(defun zc-paths/maybe-require-local ()
  "Require local configuration if presented."
  (-when-let* ((path (concat user-emacs-directory "local.el"))
               (exists (f-file? path)))
    (require 'local path)))



(provide 'zc-paths)
