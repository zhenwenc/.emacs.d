(require 's)
(require 'dash)



(defun zc-ivy/show-help ()
  (interactive)
  (let ((org-startup-folded 'nofold))
    (ivy-help)
    (pop-to-buffer (get-buffer "*Ivy Help*"))))

(defun zc-ivy/occur-then-wgrep ()
  "Shortcut for calling `ivy-occur' then activating wgrep."
  (interactive)
  (ivy-occur)
  (ivy-wgrep-change-to-wgrep-mode))

(defun zc-ivy/sort-matches-by-length (_name candidates)
  "Re-sort CANDIDATES, prioritize shorter length."
  (cl-sort (copy-sequence candidates)
           (lambda (x y)
             (let ((tx (s-trim x))
                   (ty (s-trim y)))
               (or (< (length tx) (length ty))
                   (string< tx ty))))))

(defun zc-ivy/yas-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

(defun zc-ivy/imenu (&optional widenp)
  "Replacement for `counsel-imenu'."
  (interactive)
  (cond
   ;; Similar to `counsel-imenu' but with better results
   ((derived-mode-p 'org-mode)
    (zc-org/goto-buffer-heading (if widenp 'root 'parent)))
   (t
    (call-interactively #'counsel-imenu))))

(defun zc-ivy/describe-command ()
  "Similar to `helpful-command'."
  (interactive)
  (let ((obarray (seq-filter #'commandp obarray)))
    (call-interactively #'counsel-describe-function)))



(provide 'zc-ivy-funcs)
