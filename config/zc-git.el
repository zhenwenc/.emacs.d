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
  (setq magit-bury-buffer-function    #'magit-restore-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; Display submodules information in the status buffer
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules nil t)

  ;; Add `u' command to magit-commit transient for follow-up commits
  (defun zc-git/magit-commit-update ()
    "Create a new commit on `HEAD' instantly, using the last commit message."
    (interactive)
    (magit-commit-create '("--all" "--no-verify" "--reuse-message=HEAD")))
  (transient-append-suffix 'magit-commit "c"
    '("u" "Update" zc-git/magit-commit-update)))

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


;; Interactively step forward and backwards through a buffer's history.

(use-package git-timemachine
  ;; Source code had been moved to https://codeberg.org/pidu/git-timemachine,
  ;; but MELPA still points to GitLab.
  :straight (:host github :repo "emacsmirror/git-timemachine")
  :commands git-timemachine)

;; Browse github/gitlab/bitbucket page

(use-package browse-at-remote
  :straight t
  :defer t
  :config
  ;; It raise an error when failed to infer the remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^heroku\\.com$" :type "heroku"))

  ;; Enhance remote URL lookup capability
  (defun zc-git/browse-at-remote-get-url (orig-fn &rest args)
    "Return a remote URL to browse"
    (cond
     ;; We're time traveling in the past
     ((bound-and-true-p git-timemachine-mode)
      (browse-at-remote--commit-url (car git-timemachine-revision)))
     ;; Fallback to the original function
     (t (apply orig-fn args))))
  (advice-add 'browse-at-remote-get-url :around 'zc-git/browse-at-remote-get-url)

  ;; Patch symbolic ref resolver to return tag when available
  (defun zc-git/vc-git--symbolic-ref (orig-fn filename)
    (or (funcall orig-fn filename)
        (magit-get-current-tag (vc-git-working-revision (or filename ".")))))
  (advice-add 'vc-git--symbolic-ref :around 'zc-git/vc-git--symbolic-ref)

  ;; Patch remote URL resolver to support headless ref
  (defun zc-git/browse-at-remote--remote-ref (orig-fn &optional filename)
    "Return (REMOTE-URL . REF) if symbolic ref is available,
otherwise, use the commit hash."
    (let* ((symbolic-ref (vc-git--symbolic-ref (or filename ".")))
           (browse-at-remote-prefer-symbolic symbolic-ref))
      (funcall orig-fn filename)))
  (advice-add 'browse-at-remote--remote-ref :around 'zc-git/browse-at-remote--remote-ref))

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
       ;; Branch pattern: "prefix/ABC-123/blah"
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
