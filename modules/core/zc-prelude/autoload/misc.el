;;; core/zc-prelude/autoload/misc.el -*- lexical-binding: t; -*-


;; UI

(defun zc/childframe-workable-p ()
  "Return `t' when childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))


;; Secret

(defun zc/load-private-package (pkg file)
  "Load encrypted package PKG from private directory."
  (let ((path (f-join paths-private-dir file)))
    (if (f-exists? path)
        (require pkg path)
      (warn "Private package [%s] not found." path))))

(cl-defun zc/secrets-basic-auth (&rest spec &allow-other-keys)
  (when-let ((found (car (apply 'auth-source-search :require '(:user :secret) spec)))
             (username (plist-get found :user))
             (password (funcall (plist-get found :secret))))
    (format "%s:%s" username password)))


;; Misc.

;;;###autoload
(defun zc/kill-emacs-or-frame (&optional persist-server-p)
  "Kill emacs process or the current frame."
  (interactive)
  (if persist-server-p
      (condition-case-unless-debug nil
          (delete-frame nil 1)
        (error
         (make-frame-invisible nil 1)))
    (kill-emacs)))

(defmacro zc/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))
