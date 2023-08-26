(require 'f)



(defconst paths-cache-dir
  (concat doom-user-dir ".cache/"))

(defconst paths-lisp-dir
  (concat doom-user-dir "lisp/"))

(defconst paths-vendor-dir
  (concat doom-user-dir "vendor/"))

(defconst paths-themes-dir
  (concat doom-user-dir "themes/"))

(defconst paths-private-dir
  (concat "~/dotfiles/" "private/"))



;; Load local configuration if presented.
;; This is useful when this Emacs configurations in multiple machines that have
;; different setups.
(dolist (path `("~/.emacs.local.el"
                ,(concat user-emacs-directory "local.el")))
  (when (f-exists? path) (require 'local path)))
