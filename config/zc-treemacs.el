(eval-when-compile
  (require 'use-package))

(use-package treemacs
  :straight t
  :config
  (progn
    (setq
     ;; Path to the file treemacs uses to persist its state
     treemacs-persist-file
     (f-join paths-cache-directory "treemacs-persist")

     ;; Follow the currently selected file
     treemacs-follow-after-init t

     ;; Prevents treemacs from being selected with `other-window`
     treemacs-is-never-other-window t)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)

    ;; Disable the indicator next to open files--hl-line is sufficient
    (treemacs-fringe-indicator-mode nil)

    ;; Define custom key bindings
    (zc-hydra/major-mode-define treemacs-mode
      ("Basic"
       (("?" treemacs--helpful-hydra "helpful"))

       "Project"
       (("pa" treemacs-projectile "add project")
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
