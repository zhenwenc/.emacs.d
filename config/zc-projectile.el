(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'zc-projectile-funcs)



(use-package projectile
  :straight t
  :commands (projectile-project-root
             projectile-project-name
             projectile-locate-dominating-file
             projectile-relevant-known-projects)

  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (setq projectile-enable-caching t
        projectile-kill-buffers-filter #'zc/buffer-invisible-p

        projectile-cache-file
        (concat paths-cache-dir "projectile.cache")
        projectile-known-projects-file
        (concat paths-cache-dir "projectile.projects"))

  (setq projectile-globally-ignored-files
        '("TAGS"
          ".DS_Store"
          "yarn.lock"
          "terraform.tfstate"
          "terraform.tfstate.backup"))
  (setq projectile-globally-ignored-file-suffixes
        '("gz" "zip" "tar" "elc"))
  (setq projectile-globally-ignored-directories
        '(".git"
          ".log"
          ".cache"
          ".metals"
          ".ensime_cache"
          ".serverless"
          "dist"
          "build"
          "target"
          "vendor"
          "node_modules"
          "straight/repos"))
  (setq zc-projectile/ignored-project-dirs
        '("~/.cargo" "~/.rustup"))

  (defun zc-projectile/ignore-projects-filter (dir)
    (let ((-compare-fn 'f-descendant-of?))
      (-contains? zc-projectile/ignored-project-dirs dir)))
  (setq projectile-ignored-project-function
        'zc-projectile/ignore-projects-filter)

  ;; Replace default npm project type with yarn
  (setq projectile-project-types
        (-remove (-lambda ((type)) (eq type 'npm))
                 projectile-project-types))
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "yarn build"
                                    :test "yarn test"
                                    :test-suffix ".spec")

  (advice-add 'projectile-load-known-projects
              :override #'zc-projectile/refresh-projects)

  (projectile-mode +1))

(use-package ibuffer-projectile
  :straight t
  :hook (ibuffer . zc-projectile/ibuffer-setup)
  :init
  (defun zc-projectile/ibuffer-setup ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (setq ibuffer-projectile-prefix "Project: "))

(use-package consult-projectile
  :straight (:host gitlab :repo "OlMon/consult-projectile")
  :after (consult projectile)
  :config

  (defvar zc-projectile/consult-source-package-file
    (list :name     "Package File"
          :narrow   '(?p . "Package File")
          :category 'file
          :face     'consult-file
          :history  'file-name-history
          :hidden   t ; Hidden by default, unless narrowed
          :action   (lambda (f) (consult--file-action
                                 (concat (projectile-acquire-root)
                                         (zc-projectile/find-package-root) f)))
          :enabled  #'projectile-project-root
          :items
          (lambda ()
            (-when-let* ((package-root (zc-projectile/find-package-root)))
              (->> (projectile-project-files (projectile-acquire-root))
                   (--filter (s-starts-with? package-root it))
                   (--map (s-chop-prefix package-root it)))))))

  ;; Enhanced `consult-projectile-find-file' with extra sources
  (defun zc-projectile/consult-find-file ()
    (interactive)
    (funcall-interactively
     #'consult-projectile '(consult-projectile--source-projectile-file
                            zc-projectile/consult-source-package-file))))



(provide 'zc-projectile)
