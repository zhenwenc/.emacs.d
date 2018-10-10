(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'subr-x)
(require 'seq)
(require 'f)



(defconst paths-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst paths-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst paths-config-directory
  (concat user-emacs-directory "config"))

(defconst paths-private-directory
  (concat user-emacs-directory "private"))



(defun zc-paths/init-load-paths (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs
          (list paths-lisp-directory
                paths-config-directory))
         (subdirs
          (f-directories paths-lisp-directory))
         (updated-load-path
          (seq-filter #'file-directory-p
                      (seq-uniq (append main-dirs subdirs load-path)))))

    (setq load-path updated-load-path)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
               (message "Load path updated. Added: %S" added)
               (message "No change to load-path")))))

(provide 'zc-paths)
