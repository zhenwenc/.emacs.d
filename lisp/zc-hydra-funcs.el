(require 's)
(require 'dash)
(require 'dash-functional)

(autoload 'all-the-icons-faicon "all-the-icons")
(autoload 'all-the-icons-icon-for-mode "all-the-icons")



(defun zc-hydra/title-with-faicon (name icon)
  (format "\n %s %s"
          (all-the-icons-faicon icon :v-adjust 0.01 :height 1.1)
          (propertize name 'face '(:height 1.1 :weight bold))))

(defun zc-hydra/major-mode-title (mode)
  (format "\n %s %s"
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
        (heads (zc-hydra/maybe-add-exit-head heads-plist)))
    `(pretty-hydra-define ,name
       (:hint nil :color teal :title ,title) ,heads)))

(defun zc-hydra/major-mode-hydra ()
  "Show the hydra for the current-buffer's major mode."
  (interactive)
  (let ((fname (intern (format "major-mode-hydra--%s/body" major-mode))))
    (if (fboundp fname)
        (call-interactively fname)
      (user-error "No major mode hydra for %s" major-mode))))



(provide 'zc-hydra-funcs)
