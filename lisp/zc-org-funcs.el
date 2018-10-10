(require 'f)
(require 'dash)

(autoload 'ivy-read "ivy")



(defun zc-org/read-capture-file (&rest _)
  (ivy-read "Capture Target: "
            (-map #'f-short org-agenda-files)
            :preselect org-default-notes-file
            :require-match t
            :caller 'zc-org/read-capture-target-file))

(defun zc-org/read-capture-target-file (&rest _)
  "Prompt for a target org file in `org-agenda-files'.

This is for refiling targets when doing captures.

See `org-capture-set-target-location' for example."
  (-when-let* ((path (ivy-read "Capture Target: "
                               (-map #'f-short org-agenda-files)
                               :preselect org-default-notes-file
                               :require-match t
                               :caller 'zc-org/read-capture-target-file)))
    (set-buffer (org-capture-target-buffer path))
    (org-capture-put-target-region-and-position)
    (widen)))

(defun zc-org/agenda-filter-by-category (strip)
  (interactive "P")
  (let ((cat (ivy-read "Filter Category: "
                       (-map (-compose #'f-no-ext #'f-filename) org-agenda-files)
                       :require-match t
                       :caller 'zc-org/agenda-filter-by-category)))
    (cond ((and cat strip)
           (org-agenda-filter-apply
            (push (concat "-" cat) org-agenda-category-filter) 'category))
          (cat
           (org-agenda-filter-apply
            (setq org-agenda-category-filter (list (concat "+" cat))) 'category))
          (t (error "No category provided.")))))



(provide 'zc-org-funcs)
