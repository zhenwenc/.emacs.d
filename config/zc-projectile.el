(eval-when-compile
  (require 'use-package))

(require 'zc-projectile-funcs)



(use-package projectile
  :straight t
  :commands (projectile-mode)

  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (progn
    (setq projectile-enable-caching t
          projectile-completion-system 'ivy

          projectile-cache-file
          (concat paths-cache-dir "/projectile.cache")
          projectile-known-projects-file
          (concat paths-cache-dir "/projectile.projects"))

    (setq projectile-globally-ignored-files
          '("TAGS"
            ".DS_Store"
            "terraform.tfstate"
            "terraform.tfstate.backup"))
    (setq projectile-globally-ignored-file-suffixes
          '("gz" "zip" "tar" "elc"))
    (setq projectile-globally-ignored-directories
          '(".git"
            ".ensime_cache"
            "dist"
            "build"
            "target"
            "vendor"
            "node_modules"
            "straight/repos"))

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

    (projectile-mode)))

;; counsel-projectile also loads projectile itself
(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :commands (counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-dir)

  :general (:keymaps 'projectile-command-map
                     "/"  #'counsel-projectile-rg)

  :config
  (progn
    (setq counsel-projectile-remove-current-buffer t)
    (setq counsel-projectile-remove-current-project t)

    (counsel-projectile-mode)))

(use-package all-the-icons-ivy
  :disabled t ; don't like it
  :straight t
  :after (counsel-projectile)
  :if window-system
  :config
  (progn
    (setq all-the-icons-ivy-file-commands
          '(counsel-find-file
            counsel-file-jump
            counsel-recentf
            counsel-projectile
            counsel-projectile-find-file
            counsel-projectile-find-dir))
    (all-the-icons-ivy-setup)))

(use-package ibuffer-projectile
  :straight t
  :config
  (progn
    (setq ibuffer-projectile-prefix "Project: ")
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))))



(provide 'zc-projectile)
