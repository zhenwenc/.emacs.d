;;; lisp/misc.el -*- lexical-binding: t; -*-


;; UI

(defun zc/childframe-workable-p ()
  "Return `t' when childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))


;; Debug

(defmacro zc/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))


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

(provide 'zc-misc)
