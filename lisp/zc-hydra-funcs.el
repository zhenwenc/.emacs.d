(require 's)
(require 'dash)
(require 'dash-functional)

(autoload 'all-the-icons-faicon "all-the-icons")
(autoload 'all-the-icons-icon-for-mode "all-the-icons")



(defun zc-hydra/title-with-faicon (name icon)
  (format "%s %s"
          (all-the-icons-faicon icon :v-adjust 0.01 :height 1.1)
          (propertize name 'face '(:height 1.1 :weight bold))))

(defun zc-hydra/major-mode-title (mode)
  (format "%s %s"
          (all-the-icons-icon-for-mode mode :v-adjust 0.01 :height 1.1)
          (propertize (symbol-name mode) 'face '(:height 1.1 :weight bold))))

(defun zc-hydra/remove-heads-prefix (prefix heads-plist)
  (if (stringp prefix)
      (->> heads-plist
           (--map-when (listp it)
                       (-map (-lambda ((key . rest))
                               (cons (s-trim (s-chop-prefix prefix key)) rest))
                             it)))
    heads-plist))

(defun zc-hydra/maybe-add-exit-head (heads-plist)
  (if (->> heads-plist
           (-partition 2)
           (-mapcat #'cadr)
           (-none? (-compose #'null #'cadr)))
      (--map-first (listp it)
                   (-concat it '(("q"        nil nil)
                                 ("<escape>" nil nil)))
                   heads-plist)
    heads-plist))

(defun zc-hydra/plist-remove-props (plist props)
  (->> plist
       (-partition 2)
       (--filter (not (-contains? props (car it))))
       (-flatten-n 1)))

(defun zc-hydra/normalize-head (head)
  (cond ((null head) nil)
        ;; symbol
        ((symbolp head)
         (symbol-value head))
        ;; self-evaluating form
        ((and (listp head) (functionp (car head)))
         (eval head))
        ;; function
        ((functionp head)
         `(funcall #',head))
        (t head)))



(defmacro zc-hydra/define (name body heads-plist)
  (declare (indent defun))
  (let* ((icon   (or (plist-get body :icon) "cogs"))
         (title  (zc-hydra/title-with-faicon (plist-get body :title) icon))
         (prefix (plist-get body :prefix))
         (heads  (--> heads-plist
                      (zc-hydra/remove-heads-prefix prefix it)
                      (zc-hydra/maybe-add-exit-head it)))
         (body   (zc-hydra/plist-remove-props body '(:title :icon :prefix))))
    `(pretty-hydra-define ,name (,@body :hint nil :title ,title) ,heads)))

(defmacro zc-hydra/major-mode-define (mode heads-plist)
  "Define hydra for major mode MODE with HEADS-PLIST."
  (declare (indent defun))
  (let ((name  (intern (format "major-mode-hydra--%s" mode)))
        (title (zc-hydra/major-mode-title mode))
        (heads (--> heads-plist
                    (mapcar #'zc-hydra/normalize-head it)
                    (zc-hydra/maybe-add-exit-head it))))
    `(pretty-hydra-define ,name
       (:hint nil :color teal :title ,title) ,heads)))

(defun zc-hydra/major-mode-hydra ()
  "Show the hydra for the current-buffer's major mode."
  (interactive)
  (let ((fname (intern (format "major-mode-hydra--%s/body" major-mode))))
    (if (fboundp fname)
        (call-interactively fname)
      (user-error "No major mode hydra for %s" major-mode))))



(defun use-package-normalize/:hydra (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-value))

(defun use-package-handler/:hydra (name _keyword arg rest state)
  "Handle `:hydra' keyword."
  (let ((body (use-package-process-keywords name rest state))
        (mode (use-package-as-mode name)))
    (use-package-concat
     body
     `((zc-hydra/major-mode-define ,mode ,arg)))))

(with-eval-after-load 'use-package-core
  (when (and (boundp 'use-package-keywords)
             (listp use-package-keywords))
    (add-to-list 'use-package-keywords :hydra 'append)))



(provide 'zc-hydra-funcs)
