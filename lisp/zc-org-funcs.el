(require 'f)
(require 'dash)

(autoload 'ivy-read "ivy")
(autoload 'org-agenda-filter-apply "org")
(autoload 'org-capture-target-buffer "org")
(autoload 'org-capture-put-target-region-and-position "org")
(autoload 'org-element-type "org")
(autoload 'org-element-at-point "org")

(defvar org-any-link-re)
(defvar org-ts-regexp-both)
(defvar org-tsr-regexp-both)
(defvar org-agenda-files)
(defvar org-agenda-category-filter)
(defvar org-babel-src-block-regexp)
(defvar org-default-notes-file)

(defvar zc-org/imenu-buffer-narrowed nil
  "Stores the narrowing state before `imenu' jump.")


;; General

(defun zc-org/ctrl-c-ctrl-c-hook ()
  "Override default functionality of `C-c C-c' command in
`org-mode', use with `org-ctrl-c-ctrl-c-hook'.

- When in a source code block, do edit instead of execute."
  (pcase (org-element-type (org-element-context))
    ;; source code block
    ((or `inline-src-block `src-block)
     (org-edit-special))))

(defun zc-org/evil-normal-ret ()
  "Instead of calling `org-return' when evil normal state
is actived, make it align with evil behaviour.

- When point is on an Org table, call `org-table-next-row'.
- When point is on a link, call `org-open-at-point'."
  (interactive)
  (let ((context (org-element-context)))
    (cond
     ;; In a table, all `org-table-next-row'.
     ((or (and (eq (org-element-type context) 'table)
               (>= (point) (org-element-property :contents-begin context))
               (< (point) (org-element-property :contents-end context)))
          (org-element-lineage context '(table-row table-cell) t))

      (call-interactively #'org-table-next-row))
     ;; On a link or a timestamp, call `org-open-at-point'.
     ((or (and (eq 'link (org-element-type context))
               ;; Ensure point is not on the white spaces after
               ;; the link.
               (let ((origin (point)))
                 (org-with-point-at (org-element-property :end context)
                   (skip-chars-backward " \t")
                   (> (point) origin))))
          (org-in-regexp org-ts-regexp-both nil t)
          (org-in-regexp org-tsr-regexp-both nil  t)
          (org-in-regexp org-any-link-re nil t))
      (call-interactively #'org-open-at-point))
     ;; Fallback to evil standard command
     (t
      (call-interactively #'evil-ret)))))

(defun zc-org/upcase-block-keywords (beg end)
  "Upcase Org keywords and block identifiers. The search
is restricted within the given position bounds of the buffer."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search nil)
          (count 0))
      ;; Match examples: "#+src bar", "#+src:", "=#+src=", "~#+src~",
      ;;                 "‘#+src’", "“#+src”", ",#+src bar",
      ;;                 "#+src_bar<eol>", "#+src<eol>".
      (while (re-search-forward
              "\\(?1:#\\+[a-z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)"
              end :noerror)
        (setq count (1+ count))
        (replace-match (upcase (match-string-no-properties 1))
                       :fixedcase nil nil 1))
      (message "Upper-cased %d matches" count))))

(defun zc-org/heading-folded-p ()
  "Returns non-nil if point is on a folded headline or plain
list item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

(defun zc-org/imenu-before-jump-hook (&rest _)
  "Function called before calling `imenu'."
  (setq zc-org/imenu-buffer-narrowed (buffer-narrowed-p)))

(defun zc-org/imenu-after-jump-hook ()
  "Function called after `imenu' jumping to a place."
  ;; Expand the headline if its folded
  (when (derived-mode-p 'org-mode)
    (when (zc-org/heading-folded-p)
      (org-cycle))
    (when zc-org/imenu-buffer-narrowed
      (org-narrow-to-subtree)
      (message "Buffer narrow restored!"))))


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

(defun my/org-capture-clip-snippet (file)
  "Captures the currently selected text within an org EXAMPLE
block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting file)
    (zc-org/capture-fileref-snippet file "EXAMPLE" "" nil)))

(defun zc-org/capture-code-snippet (file)
  "Captures the currently selected text within an org SRC block
with a language based on the current mode and a backlink to the
function and the file."
  (with-current-buffer (find-buffer-visiting file)
    (let* ((mode         (format "%s" major-mode))
           (org-src-mode (replace-regexp-in-string "-mode" "" mode))
           (func-name    (which-function))
           (func-link    (when func-name (format "~%s~" func-name))))
      (zc-org/capture-fileref-snippet file "SRC" org-src-mode func-link))))

(defun zc-org/capture-fileref-snippet (file type headers func-link)
  (let* ((code-snippet (when (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))))
         (file-name    (buffer-file-name))
         (file-base    (file-name-nondirectory file-name))
         (line-number  (line-number-at-pos (region-beginning)))
         (indent       "   "))
    (concat
     (concat "\n" indent)
     ;; captured file reference
     (format "From %s[[file:%s::%s][%s]]:"
             (if func-link (concat func-link " ") "")
             file-name line-number file-base)
     "\n"
     ;; selected region when capture
     (if code-snippet
         (concat
          indent "#+BEGIN_" type " " headers "\n"
          code-snippet "\n"
          indent "#+END_" type)
       ""))))

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
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun zc-org/babel-remove-result-all ()
  "Remove results from every code block in buffer."
  (interactive)
  (zc-org/babel-foreach-result 'org-babel-remove-result-one-or-many))

(defun zc-org/babel-block-in-session-p (&optional name)
  "Return if src-block is in a session of NAME.
NAME may be nil for unnamed sessions."
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))

    (cond
     ;; unnamed session, both name and session are nil
     ((and (null session)
           (null name))
      t)
     ;; Matching name and session
     ((and
       (stringp name)
       (stringp session)
       (string= name session))
      t)
     ;; no match
     (t nil))))

(defun zc-org/babel-restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (let* ((current-point (point-marker))
         (info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        ;; goto start of block
        (goto-char (match-beginning 0))
        (let* ((this-info (org-babel-get-src-block-info))
               (this-lang (nth 0 this-info))
               (this-params (nth 2 this-info))
               (this-session (cdr (assoc :session this-params))))
          (when
              (and
               (< (point) (marker-position current-point))
               (string= lang this-lang)
               (zc-org/babel-block-in-session-p session))
            (org-babel-execute-src-block arg)))
        ;; move forward so we can find the next block
        (forward-line)))))

(defun zc-org/babel-kill-session ()
  "Kill session for current code block."
  (interactive)
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (save-window-excursion
    (org-babel-switch-to-session)
    (kill-buffer)))



(provide 'zc-org-funcs)
