(require 'f)



(defconst paths-cache-dir
  (concat doom-user-dir ".cache/"))

(defconst paths-lisp-dir
  (concat doom-user-dir "lisp/"))

(defconst paths-vendor-dir
  (concat doom-user-dir "vendor/"))

(defconst paths-themes-dir
  (concat doom-user-dir "themes/"))

(defconst paths-prompts-dir
  (concat doom-user-dir "prompts/"))

(defconst paths-private-dir
  (concat "~/dotfiles/" "private/"))



;; Load local libraries.
(require 'zc-cache  (expand-file-name "lisp/cache.el"  doom-user-dir))
(require 'zc-secret (expand-file-name "lisp/secret.el" doom-user-dir))
(require 'zc-misc   (expand-file-name "lisp/misc.el"   doom-user-dir))

;; Load local configuration if presented.
;; This is useful when this Emacs configurations in multiple machines that have
;; different setups.
(dolist (path `("~/.emacs.local.el"
                ,(concat user-emacs-directory "local.el")))
  (when (f-exists? path) (require 'local path)))
