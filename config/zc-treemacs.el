(eval-when-compile
  (require 'use-package))

(require 'general)



(use-package treemacs
  :straight t
  :commands (treemacs)
  :after (:and projectile pretty-hydra)

  :hydra
  ("Basic"
   (("?" treemacs--helpful-hydra "helpful"))

   "Project"
   (("pp" treemacs-projectile                    "add project")
    ("pd" treemacs-remove-project-from-workspace "remove project")
    ("pr" treemacs-rename-project                "rename project")
    ("pc" treemacs-collapse-project              "collapse project")
    ("pC" treemacs-collapse-all-projects         "collapse all projects"))

   "Workspace"
   (("wc" treemacs-create-workspace "create workspace")
    ("wo" treemacs-switch-workspace "select workspace")
    ("wD" treemacs-remove-workspace "remove workspace")))

  :config
  (defun zc-treemacs/is-file-ignored? (file git-info)
    "Return t if FILE should not be rendered."
    (let ((-compare-fn #'f-same?))
      ;; Use treemacs function first as its faster
      (or (treemacs-is-file-git-ignored? file git-info)
          (-contains? (projectile-ignored-directories) file))))

  :config
  (setq
   ;; Path to the file treemacs uses to persist its state
   treemacs-persist-file (concat paths-cache-dir "treemacs-persist")
   ;; Follow the currently selected file
   treemacs-follow-after-init t
   ;; Prevents treemacs from being selected with `other-window`
   treemacs-is-never-other-window t)

  (treemacs-follow-mode t)
  (treemacs-git-mode 'simple)

  ;; Hide noisy files and directories
  (add-to-list 'treemacs-pre-file-insert-predicates #'zc-treemacs/is-file-ignored?)

  ;; Disable the indicator next to open files--hl-line is sufficient
  (treemacs-fringe-indicator-mode nil))

(use-package treemacs-evil
  :straight t
  :after (:and treemacs evil)
  :config
  (setq evil-treemacs-state-cursor '("SkyBlue" box))

  (general-define-key :keymaps 'treemacs-mode-map :states 'treemacs
    "h" #'treemacs-collapse-parent-node
    "l" #'treemacs-TAB-action
    "H" #'treemacs-root-up
    "L" #'treemacs-root-down))

(use-package treemacs-projectile
  :straight t
  :after (:and treemacs projectile))



(provide 'zc-treemacs)
