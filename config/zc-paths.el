(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'subr-x)
(require 'seq)
(require 'f)



(defconst paths-cache-dir
  (concat user-emacs-directory ".cache"))

(defconst paths-lisp-dir
  (concat user-emacs-directory "lisp"))

(defconst paths-config-dir
  (concat user-emacs-directory "config"))

(defconst paths-themes-dir
  (concat user-emacs-directory "themes"))

(defconst paths-private-dir
  (concat user-emacs-directory "private"))



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

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
               (message "Load path updated. Added: %S" added)
               (message "No change to load-path")))))

(provide 'zc-paths)
