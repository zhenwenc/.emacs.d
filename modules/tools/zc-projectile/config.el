;; -*- no-byte-compile: t; -*-
;;; tools/zc-projectile/config.el

(after! projectile
  (setq projectile-globally-ignored-file-suffixes
        '(".elc" ".pyc" ".o" "gz" "zip" "tar" "elc")
        projectile-globally-ignored-files
        '("TAGS"
          ".DS_Store"
          "yarn.lock"
          "terraform.tfstate"
          "terraform.tfstate.backup")
        projectile-globally-ignored-directories
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
          "straight/repos")
        zc/projectile-ignored-project-dirs '("~/.cargo" "~/.rustup")
        zc/projectile-ignored-dirs '("~/.emacs.d/straight/"))

  (setq projectile-ignored-project-function
        (-andfn 'doom-project-ignored-p
                'zc/projectile-ignore-projects-filter))
  (setq projectile-kill-buffers-filter 'zc/buffer-invisible-p)

  (advice-add 'projectile-load-known-projects
              :override #'zc/projectile-refresh-projects))

(use-package! consult-projectile
  :after (consult projectile)
  :commands (consult-projectile-find-file))
