;;; core/zc-prelude/autoload/cache.el -*- lexical-binding: t; -*-

(defclass zc/cache ()
  ((table :initarg :table)
   (test  :initarg :test)
   (ttl   :initarg :ttl)))

;;;###autoload
(cl-defun zc/make-cache (&key (test #'equal) (ttl 600))
  "Create new cache instance"
  (make-instance 'zc/cache
                 :table (make-hash-table :test test)
                 :test test :ttl ttl))

;;;###autoload
(defun zc/cache-rem (cache key)
  "Remove KEY from CACHE"
  (with-slots (table) cache
    (remhash key table)))

;;;###autoload
(cl-defun zc/cache-get (cache key)
  "Return cached VALUE for KEY and discard expired item"
  (with-slots (table) cache
    (if-let* ((found (gethash key table)))
        (when (< (time-to-seconds) (plist-get found :exp))
          (plist-get found :value))
      (remhash key table))))

;;;###autoload
(defun zc/cache-set (cache key value &optional &key ttl)
  "Store VALUE with KEY in CACHE with TTL"
  (with-slots (table test (default-ttl ttl)) cache
    (puthash key (list :value value
                       :exp (+ (time-to-seconds) (or ttl default-ttl)))
             table)))

;;;###autoload
(cl-defmacro zc/cached (cache key &rest body)
  "Return cached VALUE for KEY if exists, otherwise resolve value with
BODY and store the returned value into CACHE with TTL."
  `(or (zc/cache-get ,cache ,key)
       (let ((value (progn ,@body)))
         (zc/cache-set ,cache ,key value)
         value)))
