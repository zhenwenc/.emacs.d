(require 'f)
(require 'dash)

(autoload 'ivy-read "ivy")
(autoload 'org-element-type "org")
(autoload 'org-element-at-point "org")
(autoload 'org-agenda-filter-apply "org")
(autoload 'org-capture-target-buffer "org")
(autoload 'org-capture-put-target-region-and-position "org")

(defvar org-any-link-re)
(defvar org-ts-regexp-both)
(defvar org-tsr-regexp-both)
(defvar org-agenda-files)
(defvar org-agenda-category-filter)
(defvar org-babel-src-block-regexp)
(defvar org-default-notes-file)
(defvar org-default-babel-file)
(defvar org-work-notes-file)
(defvar counsel-outline-settings)
(defvar counsel-outline--preselect)

(defconst zc-org/directory "~/notes")


;; General

(defun zc-org/file-with-exts (exts &optional dir)
  "Return files in `org-directory' that matches extension in EXTS."
  (unless dir (setq dir zc-org/directory))
  (f-files dir (-compose (-partial #'-contains? exts) #'f-ext) nil))

(defun zc-org/evil-normal-ret ()
  "Instead of calling `org-return' when evil normal state
is actived, make it align with evil behaviour.

- When point is on an source block, call `org-babel-execute-src-block'.
- When point is on an `:+CALL:' block, call `org-babel-execute-maybe'.
- When point is on an Org table, call `org-table-next-row'.
- When point is on a link, call `org-open-at-point'."
  (interactive)
  (let ((context (org-element-context)))
    (cond
     ;; In a source block, call `org-babel-execute-src-block'.
     ((org-in-src-block-p)
      (org-babel-eval-wipe-error-buffer)
      (org-babel-execute-src-block current-prefix-arg))
     ;; In a `:+CALL:' block, call `org-babel-execute-maybe'.
     ((eq (org-element-type context) 'babel-call)
      (call-interactively #'org-babel-execute-maybe))
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

(defun zc-org/heading-or-item-folded-p ()
  "Returns non-nil if point is on a folded headline or plain
list item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

(defun zc-org/export-pdf-and-open ()
  "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
  (interactive)
  (save-buffer)
  (let* ((pdf-path (org-latex-export-to-pdf))
         (pdf-name (file-name-nondirectory pdf-path)))
    (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
        (progn
          (kill-matching-buffers (concat "^" pdf-name) t t)
          (find-file-other-window pdf-name))
      (find-file-other-window pdf-name))
    (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex"))))


;; Navigation

(defun zc-org/goto-buffer-heading (&optional tree)
  "Jump to an outline heading within the current buffer.

Filter candidates with TREE:
- When equals to 'parent, show all siblings recursively.
- Otherwise, show all headings in the buffer.

See also `counsel-outline'."
  (interactive)
  (let* ((settings (cdr (assq major-mode counsel-outline-settings)))
         (candidates (zc/with-wide-buffer
                      (pcase tree
                        ('parent (ignore-errors (outline-up-heading 1))
                                 (org-narrow-to-subtree)))
                      (counsel-outline-candidates settings))))
    (ivy-read "Outline: " candidates
              :action  (or (plist-get settings :action) #'counsel-outline-action)
              :history (or (plist-get settings :history) 'counsel-outline-history)
              :preselect (max (1- counsel-outline--preselect) 0)
              :caller 'zc-org/goto-with-widen-buffer)))

(defun zc-org/goto-file-heading (type)
  "Jump to a heading in an org file.

See also `counsel-org-goto-all'."
  (interactive)
  (let ((files (pcase type
                 ('notes (list org-default-notes-file org-work-notes-file))
                 ('babel (list org-default-babel-file))
                 (_ org-agenda-files))))
    (ivy-read "Goto: " (zc-org/get-outline-candidates files)
              :history 'counsel-org-goto-history
              :action #'zc-org/goto-file-heading-action
              :caller #'zc-org/goto-file-heading)))

(defun zc-org/goto-file-heading-action (x)
  "Jump to headline in candidate X.

Ensure we are are in `org' layout to avoid chaos"
  (unless (projectile-ensure-project zc-org/directory)
    (error "Org directory '%s' is not a project" zc-org/directory))
  (let* ((marker (cdr x))
         (project zc-org/directory))
    (-if-let* ((is-marker (markerp marker))
               (buffer    (marker-buffer marker)))
        (zc-projectile/with-switch-project-action buffer
          (zc-layout/create-project-layout project))
      (zc-layout/create-project-layout project)))
  (counsel-org-goto-action x))

(defun zc-org/get-outline-candidates (filenames)
  "Return an alist of counsel outline heading completion candidates,
using `counsel-outline-candidates'.

Each element is a pair (HEADING . MARKER), where the string HEADING
is located at the position of MARKER."
  (->> filenames
       (-filter #'f-exists?)
       (-map-when (-compose #'not #'bufferp)
                  (-rpartial #'find-file-noselect t))
       ;; Collect headline candidates
       (mapcan (lambda (buffer)
                 (with-current-buffer buffer
                   (zc/with-wide-buffer
                    (counsel-outline-candidates)))))
       ;; Prepend the file name
       (-map (-lambda ((head . marker))
               (--> marker
                    (buffer-file-name (marker-buffer it))
                    (f-relative it zc-org/directory)
                    (f-no-ext it)
                    (propertize it 'face 'ivy-virtual)
                    (concat "[" it "] " head)
                    (cons it marker))))))


;; Hooks and Advices

(defun zc-org/narrow-after-jump (&rest _)
  "Function called after org jumping to a location.

Expand the headline and narrow to current subtree."
  (when (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (outline-hide-other)
    (outline-show-subtree)
    (org-narrow-to-subtree)
    (message "Narrowed to subtree!")))


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
          (t (error "No category provided")))))


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

(defun zc-org/babel-confirm-evaluate (lang body)
  "Function for `org-confirm-babel-evaluate' to determin the
source code block should be executed."
  ;; If `EVAL_NO_CONFIRM: yes' is defined
  (pcase (org-entry-get (point) "eval_no_confirm" t)
    ("yes" nil)
    ((or "no" `nil) (string-equal lang "shell"))
    (_ (error "Invalid 'EVAL_NO_CONFIRM' value, use yes/no"))))

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
    (kill-current-buffer)))


;; Smartparens

(defun zc-org/sp-point-in-checkbox-p (_id action _context)
  (and (eq action 'insert)
       (sp--looking-at-p "\\s-*]")))

(defun zc-org/sp-point-at-bol-p (_id action _context)
  (and (eq action 'insert)
       (eq (char-before) ?*)
       (sp--looking-back-p "^\\**" (line-beginning-position))))



(provide 'zc-org-funcs)
