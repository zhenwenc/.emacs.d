(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'dash)
(require 'dash-functional)
(require 'general)



(defun zc/projectile-search-symbol-at-point (current-dir-p)
  (interactive "P")
  (let ((sym (thing-at-point 'symbol t)))
    (if (and (projectile-project-p) (not current-dir-p))
        (let ((counsel-projectile-rg-initial-input sym))
          (counsel-projectile-rg))
      (counsel-rg sym default-directory ""
                  (format "[%s] rg" default-directory)))))



(use-package projectile
  :straight t
  :commands (projectile-mode)

  :preface
  (progn
    (defconst zc-projectile/ignored-dirs
      '("~/.emacs.d/straight/"))

    (defun zc-projectile/ignored-project-p (project)
      (thread-last zc-projectile/ignored-dirs
        (-map #'file-truename)
        (-any? (-orfn
                (-rpartial #'f-same? project)
                (-rpartial #'f-ancestor-of? project)))))

    (defun zc-projectile/refresh-projects ()
      "Update `projectile-known-projects', append magit discovered
repos, and remove projects if non-exist or is the directory or sub-
directory in `zc-projectile/ignored-dirs'."
      (interactive)
      (when (require 'magit nil t)
        ;; Add all projects that detected by magit
        (mapc #'projectile-add-known-project
              (mapcar (-compose #'file-name-as-directory #'f-abbrev)
                      (magit-list-repos)))
        ;; Filter the known projects
        (thread-last projectile-known-projects
          (-remove (-compose
                    #'zc-projectile/ignored-project-p
                    #'file-truename))
          (setq projectile-known-projects))
        ;; Ensure the projects exist
        (projectile-cleanup-known-projects))))

  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)

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

    (advice-add 'projectile-load-known-projects
                :override #'zc-projectile/refresh-projects)

    ;; Teach projectile to prefer rg for finding files containing strings
    (advice-add 'projectile-files-with-string
                :around #'config-projectile--find-files-with-string-using-rg)

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

    ;; The default counsel rg command ending with a dot, this will
    ;; produce duplicated result for `counsel-projectile-rg'.
    (setq counsel-rg-base-command
          (s-replace-regexp "[ \t\n\r]+\\.?\\'" "" counsel-rg-base-command))

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
