;;; lisp/secret.el -*- lexical-binding: t; -*-

;; Remarks:
;;
;; The decrypted authentication information are cached, if corrupted,
;; execute `auth-source-forget-all-cached' to refresh the cache.
;;

(defun zc/load-private-package (pkg file)
  "Load encrypted package PKG from private directory."
  (let ((path (f-join paths-private-dir file)))
    (if (f-exists? path)
        (require pkg path)
      (warn "Private package [%s] not found." path))))

(cl-defun zc/secrets-basic-auth (&rest spec &allow-other-keys)
  (auth-source-forget-all-cached)
  (when-let ((found (car (apply 'auth-source-search :require '(:user :secret) spec)))
             (username (plist-get found :user))
             (password (funcall (plist-get found :secret))))
    (format "%s:%s" username password)))

(cl-defun zc/secrets-api-key (&rest spec &allow-other-keys)
  (auth-source-forget-all-cached)
  (when-let ((found (car (apply 'auth-source-search :require '(:secret) spec)))
             (token (funcall (plist-get found :secret))))
    token))

(provide 'zc-secret)
