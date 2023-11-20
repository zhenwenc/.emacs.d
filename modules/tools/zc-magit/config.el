(after! magit
  (setq magit-repository-directories
        '(("~/.emacs.d/" . 0)
          ("~/.doom.d/"  . 0)
          ("~/code/"     . 2)
          ("~/work/"     . 1)
          ("~/notes/"    . 0)
          ("~/dotfiles/" . 0)))

  ;; Display magit buffer in fullframe
  ;; TODO Is this correct? Does doom happy?
  (setq magit-bury-buffer-function    #'magit-restore-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; Disable the auto-revert buffer feature for certain major modes
  (defun zc/magit-revert-buffer-override (orig-fn buffer)
    (unless (or (not buffer-file-name)
                (f-ext? buffer-file-name ".gpg")
                (eq major-mode 'org-mode))
      (funcall orig-fn buffer)))
  (advice-add #'+magit--revert-buffer :around #'zc/magit-revert-buffer-override))

;; UX: Disable in Org mode, the performance is extremely horrible
(after! (magit git-gutter)
  (add-to-list 'git-gutter:disabled-modes 'org-mode t))

;; Generate URL with tag or branch name when available
(after! browse-at-remote
  (setq browse-at-remote-prefer-symbolic t))


;;; Functions

(defun zc/projectile-ignore-projects-filter (dir)
  (let ((-compare-fn 'f-descendant-of?))
    (-contains? zc/projectile-ignored-project-dirs dir)))

;; Add `u' command to magit-commit transient for follow-up commits
(defun zc-git/magit-commit-update ()
  "Create a new commit on `HEAD' instantly, using the last commit message."
  (interactive)
  (magit-commit-create '("--all" "--no-verify" "--reuse-message=HEAD")))
(after! magit
  (transient-append-suffix 'magit-commit "c" '("u" "Update" zc-git/magit-commit-update)))


;; Automatically prepends the JIRA ticket number to commit messages
;;
;; This utility was inspired by `git-commit-jira-prefix', but diverged overtime
;; in terms of the prefix pattern, and due to compatibility issue with Doom which
;; comes with a similar purposed hook `+vc-start-in-insert-state-maybe-h'.

(defun zc-git/commit-prefix ()
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

(add-hook! 'git-commit-setup-hook
  (defun zc-git/commit-prefix-h ()
    (-when-let (ticket (zc-git/commit-prefix))
      ;; Insert the prefix string
      (goto-char (point-min))
      (goto-char (line-beginning-position))
      (insert (format "%s " ticket))
      ;; Move cursor back to the end of first line
      (goto-char (point-min))
      (goto-char (line-end-position))
      (just-one-space))))



(provide 'zc-git)
