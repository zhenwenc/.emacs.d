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
  (let ((name  (zc-hydra/major-mode-name mode))
        (title (zc-hydra/major-mode-title mode))
        (heads (--> heads-plist
                    (zc-hydra/maybe-add-exit-head it))))
    `(pretty-hydra-define+ ,name
       (:hint nil :color teal :title ,title) ,heads)))

(defun zc-hydra/major-mode-hydra ()
  "Show the hydra for the current-buffer's major mode."
  (interactive)
  (let ((fname (zc-hydra/major-mode-name major-mode "body")))
    (if (fboundp fname)
        (call-interactively fname)
      (user-error "No major mode hydra for %s" major-mode))))

(defun zc-hydra/major-mode-name (mode &optional fn)
  "Return hydra symbol for the major mode MODE."
  (intern (format "major-mode-hydra--%s"
                  (s-join "/" (remove nil (list (symbol-name mode) fn))))))

(defmacro zc-hydra/require-after-load (features* body)
  "Define hydra after FEATURES* are loaded, accepts same
arguments as `:after' directive in `use-package'."
  (declare (indent defun))
  (if (null features*)
      body
    `(use-package-require-after-load ,features* ,body)))



(defun use-package-normalize/:hydra (package keyword args)
  (-map
   (lambda (arg)
     (cond ((stringp (car arg)) ; only hydra heads
            `(nil nil nil ,arg nil))
           ((= 2 (length arg))  ; major mode hydra + heads
            (-let [((meta &as &plist :name name :mode mode) heads) arg]
              (unless (or name mode)
                (use-package-error "hydra wants name or major mode"))
              (unless (and mode (stringp (car heads)))
                (use-package-error "hydra wants body list"))
              `(,name ,mode nil ,heads ,meta)))
           ((= 3 (length arg))  ; meta + hydra body + heads
            (-let [((meta &as &plist :name name :mode mode) body heads) arg]
              (unless (plist-get body :title)
                (use-package-error "hydra body wants title"))
              `(,name ,mode ,body ,heads ,meta)))
           (t (use-package-error "invalid hydra arguments"))))
   args))

(defun use-package-handler/:hydra (package _keyword args rest state)
  "Handle `:hydra' keyword."
  (use-package-concat
   (use-package-process-keywords package rest state)
   (-mapcat
    (-lambda ((name modes body heads (&plist :after after)))
      ;; Inhibit specifying hydra name when defining major mode hydra.
      (when (and name modes)
        (use-package-error ":name is not allowed for major mode hydra."))
      ;; When `:name' is specified, defines normal hydra,
      ;; otherwise major mode hydra will be defined. A major
      ;; mode will be inferred from the package name if none
      ;; is given.
      (unless (or name modes)
        (setq modes (list (use-package-as-mode package))))
      (zc-hydra/require-after-load after
        (if modes
            (-map (lambda (mode)
                    `(zc-hydra/major-mode-define ,mode ,heads))
                  (-list modes))
          `((zc-hydra/define ,name ,body ,heads)))))
    args)))

(with-eval-after-load 'use-package-core
  (when (and (boundp 'use-package-keywords)
             (listp use-package-keywords))
    (add-to-list 'use-package-keywords :hydra 'append)))



(provide 'zc-hydra-funcs)
