(require 'f)
(require 'dash)
(require 'eshell)

(autoload 'shrink-path-file "shrink-path")

(defvar eshell-buffer-name)
(defvar eshell-last-output-end)
(defvar eshell-last-command-status)



(defface zc-eshell/prompt-pwd nil
  "Face for eshell prompt pwd."
  :group 'eshell)

(defface zc-eshell/prompt-git-branch nil
  "Face for eshell prompt git branch."
  :group 'eshell)



(defun zc-eshell/prompt-pwd ()
  "Returns fish-style trunctated current directories."
  (let ((pwd (eshell/pwd)))
    (if (equal pwd "~") pwd
      (abbreviate-file-name (shrink-path-file pwd)))))

(defun zc-eshell/prompt-git-branch ()
  "Returns the current git branch if in project."
  (let* ((blines (split-string (shell-command-to-string "git branch") "\n"))
         (branch (car (cl-loop for match in blines
                               if (string-match-p "^\*" match)
                               collect match))))
    (if (not (eq branch nil))
        (format " [%s]" (substring branch 2))
      "")))


;; Commands

;;;###autoload
(defun zc-eshell/open (arg &optional command newp)
  "Open eshell. With ARG, change to buffer's directory."
  (interactive "P")
  (when (eq major-mode 'eshell-mode)
    (user-error "Already in an eshell buffer"))
  (let* ((dir (or (unless arg (projectile-project-root))
                  default-directory))
         (buf (if (and newp (buffer-live-p eshell-buffer-name))
                  (generate-new-buffer eshell-buffer-name)
                eshell-buffer-name)))
    (with-current-buffer (pop-to-buffer buf)
      (unless (eq major-mode 'eshell-mode) (eshell))
      (when arg (eshell/cd dir) (eshell-reset t))
      (when command
        (eshell-kill-input)
        (insert command)
        (eshell-send-input nil t)))))

;;;###autoload
(defun zc-eshell/prompt ()
  "Generate the prompt string for `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (propertize (zc-eshell/prompt-pwd) 'face 'zc-eshell/prompt-pwd)
          (propertize (zc-eshell/prompt-git-branch)
                      'face 'zc-eshell/prompt-git-branch)
          (propertize " λ" 'face (if (eshell-exit-success-p) 'success 'error))
          " "))

(defun zc-eshell/cleanup ()
  "Close window after `eshell/exit'."
  (-when-let* ((buf (current-buffer))
               (win (get-buffer-window buf)))
    (unless (one-window-p)
      (let ((ignore-window-parameters t))
        (delete-window win)))))

;;;###autoload
(defun zc-eshell/goto-prompt-on-insert ()
  "Move cursor to the prompt when switching to insert mode."
  (when (< (point) eshell-last-output-end)
    (goto-char
     (if (memq this-command '(evil-append evil-append-line))
         (point-max)
       eshell-last-output-end))))

;;;###autoload
(defun zc-eshell/goto-end-of-prompt ()
  "Move cursor to the end of prompt."
  (interactive)
  (goto-char (point-max))
  (evil-append 1))

;;;###autoload
(defun zc-eshell/kill-and-close ()
  "Kill the current eshell buffer and close its window."
  (interactive)
  (unless (eq major-mode 'eshell-mode)
    (user-error "Not in an eshell buffer"))
  (kill-this-buffer))



(provide 'zc-eshell-funcs)
