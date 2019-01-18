(require 'f)
(require 'dash)

(defvar org-agenda-files)
(defvar org-default-notes-file)
(defvar org-agenda-category-filter)

(autoload 'ivy-read "ivy")
(autoload 'org-agenda-filter-apply "org")
(autoload 'org-capture-target-buffer "org")
(autoload 'org-capture-put-target-region-and-position "org")
(autoload 'org-element-type "org")
(autoload 'org-element-at-point "org")


;; General

(defun zc-org/ctrl-c-ctrl-c-hook ()
  "Override default functionality of `C-c C-c' command in
`org-mode', use with `org-ctrl-c-ctrl-c-hook'.

- When in a source code block, do edit instead of execute."
  (pcase (org-element-type (org-element-context))
    ;; source code block
    ((or `inline-src-block `src-block)
     (org-edit-special))))


;; Agenda

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


;; Babel

(defun zc-org/babel-foreach-result (fn)
  "Run FN for each source block in buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^\s*#[+]BEGIN_SRC" nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun zc-org/babel-remove-result-all ()
  (interactive)
  (zc-org/babel-foreach-result 'org-babel-remove-result-one-or-many))



(provide 'zc-org-funcs)
