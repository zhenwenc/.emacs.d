(eval-when-compile
  (require 'use-package))



(use-package magit
  :straight t

  :general (:states 'normal :keymaps 'magit-status-mode-map
            "q" #'magit-mode-bury-buffer)

  :config
  (setq magit-repository-directories
        '(("~/.emacs.d/" . 0)
          ("~/code/"     . 2)
          ("~/work/"     . 1)
          ("~/notes/"    . 0)
          ("~/dotfiles/" . 0)))

  ;; Display magit buffer in fullframe
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Show source files' todos in Magit status buffer

(use-package magit-todos
  :straight t
  :disabled t
  :hook (emacs-startup . magit-todos-mode))


;; Transient commands (previously known as magit-popup)

(use-package transient
  :straight t
  :general (:keymaps 'transient-map
            "<escape>" #'transient-quit-one
            "q"        #'transient-quit-one)
  :init
  (setq transient-levels-file  (concat paths-cache-dir "transient/levels.el")
        transient-values-file  (concat paths-cache-dir "transient/values.el")
        transient-history-file (concat paths-cache-dir "transient/history.el")))


;; Interfaces to GitHub integration

(use-package forge
  :straight t
  :after magit
  :commands magit-status
  :init
  (setq forge-database-file (concat paths-cache-dir "forge/database.sqlite")))


;; Interactively step forward and backwards through a
;; buffer's git versions.

(use-package git-timemachine
  :straight t
  :defer t)

;; Browse github/gitlab/bitbucket page

(use-package browse-at-remote
  :straight t
  :defer t)

;; Automatically prepends the JIRA ticket number

(use-package git-commit-jira-prefix
  :straight (:host github :repo "chrisbarrett/git-commit-jira-prefix")
  :after git-commit
  :config
  (git-commit-jira-prefix-init)

  (defun zc-git/commit-prefix (&rest _ignored)
    (when-let ((branch (magit-get-current-branch))
               (is-blank (s-blank? (save-excursion
                                     (goto-char (point-min))
                                     (buffer-substring (line-beginning-position)
                                                       (line-end-position))))))
      (or
       ;; Branch pattern: "feature/ABC-123/blah"
       (-when-let* ((branch-ptn (rx bos (+ alpha)
                                    "/" (group (+ upper) "-" (+ num))
                                    "/" (+ (or alpha "-"))))
                    ((_ ticket) (s-match branch-ptn branch)))
         (concat "[" ticket "]"))
       ;; Branch pattern: "prefix/ABC-123-<type>-blah"
       (-when-let* ((staged (f-common-parent (magit-staged-files)))
                    (branch-ptn (rx bos (+ alpha)
                                    "/" (group (+ upper) "-" (+ num))
                                    "-" (group (+ lower)) "-"))
                    (module-ptn (rx (+ alpha)
                                    "/" (group (+ (or lower "-")))))
                    ((_ ticket type) (s-match branch-ptn branch))
                    ((_ module)      (s-match module-ptn staged)))
         (s-join "\n\n" (list (concat (or type "feat") "(" module "): ") ticket)))
       ;; Fixed commit messages for notes
       (when (f-equal-p (projectile-project-root) zc-org/directory) "update"))))
  (advice-add 'git-commit-jira-prefix--ticket-number :override 'zc-git/commit-prefix))



(use-package vc-annotate
  :commands (vc-annotate)
  :general (:states 'normal :keymaps 'vc-annotate-mode-map
            "n" 'vc-annotate-next-revision
            "f" 'vc-annotate-next-revision
            "p" 'vc-annotate-prev-revision
            "b" 'vc-annotate-prev-revision
            "." 'vc-annotate-working-revision))



(provide 'zc-git)
