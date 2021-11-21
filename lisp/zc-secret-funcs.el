(require 'f)
(require 's)
(require 'zc-paths)



(defun zc/load-private-package (pkg file)
  "Load encrypted package PKG from private directory."
  (let ((path (f-join paths-private-dir file)))
    (if (f-exists? path)
        (require pkg path)
      (warn "Private package [%s] not found." path))))


;; Auth Source

(cl-defun zc/secrets-basic-auth (&rest spec &allow-other-keys)
  (when-let ((found (car (apply 'auth-source-search :require '(:user :secret) spec)))
             (username (plist-get found :user))
             (password (funcall (plist-get found :secret))))
    (format "%s:%s" username password)))



(provide 'zc-secret-funcs)
