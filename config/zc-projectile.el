(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'zc-projectile-funcs)



(use-package projectile
  :straight t
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)

  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
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



(provide 'zc-projectile)
