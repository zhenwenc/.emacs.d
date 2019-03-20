(eval-when-compile
  (require 'use-package))

(use-package treemacs
  :straight t
  :after projectile

  :preface
  (defun zc-treemacs/is-file-ignored? (file git-info)
    "Return t if FILE should not be rendered."
    (let ((-compare-fn #'f-same?))
      ;; Use treemacs function first as its faster
      (or (treemacs-is-file-git-ignored? file git-info)
          (-contains? (projectile-ignored-directories) file))))

  :config
  (progn
    (setq
     ;; Path to the file treemacs uses to persist its state
     treemacs-persist-file (f-join paths-cache-directory "treemacs-persist")
     ;; Follow the currently selected file
     treemacs-follow-after-init t
     ;; Prevents treemacs from being selected with `other-window`
     treemacs-is-never-other-window t)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)

    ;; Hide noisy files and directories
    (add-to-list 'treemacs-pre-file-insert-predicates
                 #'zc-treemacs/is-file-ignored?)

    ;; Disable the indicator next to open files--hl-line is sufficient
    (treemacs-fringe-indicator-mode nil)

    ;; Define custom key bindings
    (zc-hydra/major-mode-define treemacs-mode
      ("Basic"
       (("?" treemacs--helpful-hydra "helpful"))

       "Project"
       (("pp" treemacs-projectile "add project")
        ("pd" treemacs-remove-project-from-workspace "remove project")
        ("pr" treemacs-rename-project "rename project")
        ("pc" treemacs-collapse-project "collapse project")
        ("pC" treemacs-collapse-all-projects "collapse all projects"))

       "Workspace"
       (("wc" treemacs-create-workspace "create workspace")
        ("wo" treemacs-switch-workspace "select workspace")
        ("wD" treemacs-remove-workspace "remove workspace"))))
    ))

(use-package treemacs-evil
  :straight t
  :after (:and treemacs evil)
  :config
  (setq evil-treemacs-state-cursor '("SkyBlue" box)))

(use-package treemacs-projectile
  :straight t
  :after (:and treemacs projectile))

(provide 'zc-treemacs)
