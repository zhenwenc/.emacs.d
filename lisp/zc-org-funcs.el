(require 'f)
(require 'dash)
(require 'org)
(require 'org-element)

(autoload 'ivy-read "ivy")
(autoload 'counsel-org-goto-action "counsel")
(autoload 'counsel-outline-candidates "counsel")
(autoload 'projectile-ensure-project "projectile")
(autoload 'consult-org-heading "consult-org")
(autoload 'consult-org--headings "consult-org")

(defvar org-any-link-re)
(defvar org-ts-regexp-both)
(defvar org-tsr-regexp-both)
(defvar org-agenda-files)
(defvar org-agenda-category-filter)
(defvar org-babel-src-block-regexp)
(defvar org-babel-result-regexp)
(defvar org-default-notes-file)
(defvar org-default-babel-file)
(defvar counsel-outline-settings)
(defvar counsel-outline--preselect)
(defvar counsel-outline-path-separator)

(defconst zc-org/directory "~/notes")
(defconst zc-org/main-notes-dir (f-join zc-org/directory "main"))
(defconst zc-org/work-notes-dir (f-join zc-org/directory "work"))


;; General

(cl-defun zc-org/file-with-exts (&key ext dir)
  "Return files in `org-directory' that matches extension in EXTS."
  (unless dir (setq dir zc-org/directory))
  (unless ext (setq ext '("org")))
  (f-files dir (-compose (-partial #'-contains? ext) #'f-ext) nil))

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
  "Run `org-latex-export-to-pdf', delete the tex file and open
pdf in a new buffer.

- Full installation:
  brew install --cask mactex

- Full installation without bundled applications:
  brew install --cask mactex-no-gui

- Minimal installation:
  brew install --cask basictex
"
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

(defun zc-org/outline-buffer-heading (&optional scope)
  "Jump to an outline heading within the current buffer. This
function pushes the current subtree to mark ring, so that you
can jump back using `org-mark-ring-goto'.

Filter candidates with SCOPE, see valid options in `org-map-entries'. By
default only siblings are available.

See also `counsel-outline' and `consult-org-heading'."
  (interactive (unless (derived-mode-p 'org-mode)
                 (user-error "Must be called from an Org buffer")))
  (let* ((candidates (pcase scope
                       ('parent (zc/with-widen-buffer
                                 (ignore-errors (outline-up-heading 1)
                                                (org-narrow-to-subtree))
                                 (consult-org--headings t nil 'tree)))
                       ;; HACK: Cannot use 'file scope on indirect buffers
                       (_ (with-current-buffer (or (buffer-base-buffer)
                                                   (current-buffer))
                            (consult-org--headings t nil 'file)))))

         ;; FIXME workaround with `counsel-outline-candidates' due to problematic
         ;; behaviours in `consult-org--headings' that causes Emacs to freeze.
         ;; (buffer (or (buffer-base-buffer) (current-buffer)))
         ;; (buffer-name (buffer-name buffer))
         ;; (settings (cdr (assq major-mode counsel-outline-settings)))
         ;; (candidates (->> (zc/with-widen-buffer
         ;;                   (pcase scope
         ;;                     ('parent (ignore-errors (outline-up-heading 1)
         ;;                                             (org-narrow-to-subtree))))
         ;;                   (counsel-outline-candidates settings))
         ;;                  (-map (-lambda ((cand . marker))
         ;;                          (setq cand (format "%s %s" buffer-name cand))
         ;;                          (add-text-properties
         ;;                           0 1 `(consult--candidate ,marker) cand)
         ;;                          cand))))

         (selected (consult--read
                    candidates
                    :prompt "Go to heading: "
                    :category 'consult-org-heading
                    :sort nil
                    :require-match t
                    :narrow (consult-org--narrow)
                    :lookup #'consult--lookup-candidate
                    :group #'zc-org/outline-group-by-buffer)))
    ;; Jump to headline in selected candidate position.
    (let ((narrowed (buffer-narrowed-p)))
      ;; Push current subtree to mark ring, see `org-mark-subtree'.
      (org-with-limited-levels
       (cond ((org-at-heading-p) (beginning-of-line))
             ((not (org-before-first-heading-p))
              (outline-previous-visible-heading 1)))
       (org-mark-ring-push))
      ;; HACK: The candidates are collected from the original buffer
      (-if-let* ((base (buffer-base-buffer))
                 (mark (copy-marker (marker-position selected) base)))
          (org-goto-marker-or-bmk mark)
        (org-goto-marker-or-bmk selected))
      (when narrowed (zc-org/narrow-to-subtree)))))

(defun zc-org/outline-file-heading (&optional scope)
  "Jump to a outline heading in a directory.

See also `counsel-org-goto-all'."
  (interactive)
  (let* ((files (pcase scope
                  ('note  (f-files zc-org/main-notes-dir (-rpartial #'f-ext-p "org")))
                  ('work  (f-files zc-org/work-notes-dir (-rpartial #'f-ext-p "org")))
                  ('babel (list org-default-babel-file))))
         (candidates (consult-org--headings t nil files))
         (selected (consult--read candidates
                                  :prompt "Go to heading: "
                                  :category 'consult-org-heading
                                  :sort nil
                                  :require-match t
                                  :narrow (consult-org--narrow)
                                  :lookup #'consult--lookup-candidate
                                  :group #'zc-org/outline-group-by-buffer)))
    ;; Ensure we are are in `org' layout to avoid chaos.
    (unless (projectile-ensure-project zc-org/directory)
      (error "Org directory '%s' is not a project" zc-org/directory))
    (let* ((marker selected)
           (project zc-org/directory))
      (-if-let* ((is-marker (markerp marker))
                 (buffer    (marker-buffer marker)))
          (zc-projectile/with-switch-project-action buffer
            (zc-layout/create-project-layout project))
        (zc-layout/create-project-layout project)))
    (org-with-limited-levels
     (cond ((org-at-heading-p) (beginning-of-line))
           ((not (org-before-first-heading-p))
            (outline-previous-visible-heading 1)))
     (org-mark-ring-push))
    (org-goto-marker-or-bmk selected)
    (zc-org/narrow-to-subtree)))

(defun zc-org/outline-group-by-buffer (cand transform)
  "The default grouping logic provided by `consult-org-heading'."
  (let* ((buf (get-text-property 0 'consult--candidate cand))
         (name (buffer-name (marker-buffer buf))))
    (if transform (substring cand (1+ (length name))) name)))

(defun zc-org/outline-candidates (&optional filenames)
  "Return an alist of counsel outline heading completion candidates,
using `counsel-outline-candidates'.

Each element is a pair (HEADING . MARKER), where the string HEADING
is located at the position of MARKER."
  (->> (or filenames org-agenda-files)
       (-filter #'f-exists?)
       (-map-when (-compose #'not #'bufferp)
                  (-rpartial #'find-file-noselect t))
       ;; Collect headline candidates
       (mapcan (lambda (buffer)
                 (with-current-buffer buffer
                   (zc/with-widen-buffer
                    (counsel-outline-candidates
                     (cdr (assq 'org-mode counsel-outline-settings)))))))
       ;; Filter candidates by maximum headline depth
       (-filter (-lambda ((head . marker))
                  (< (length (s-split counsel-outline-path-separator head)) 4)))
       ;; Prepend the file name
       (-map (-lambda ((head . marker))
               (--> marker
                    (buffer-file-name (marker-buffer it))
                    (f-relative it zc-org/directory)
                    (f-no-ext it)
                    (propertize it 'face 'ivy-virtual)
                    (concat "[" it "] " head)
                    (cons it marker))))))

(defun zc-org/outline-previous-mark (&optional n)
  "Enhanced `org-mark-ring-goto' to break narrowed buffer."
  (interactive "p")
  (let ((narrowed (buffer-narrowed-p)))
    ;; Break buffer narrow boundary
    (when narrowed (widen))
    (funcall-interactively 'org-mark-ring-goto n)
    (when narrowed (org-narrow-to-subtree)))
  ;; Show all direct subheadings of this heading
  (org-show-children))

(defun zc-org/outline-up-heading (arg)
  "Move to the previous (possibly invisible) heading line.
Enhanced `outline-up-heading' to break narrowed buffer."
  (interactive "p")
  (let ((narrowed (buffer-narrowed-p)))
    ;; Break buffer narrow boundary
    (when narrowed (widen))
    (funcall-interactively 'outline-up-heading arg t)
    (when narrowed (org-narrow-to-subtree)))
  ;; Show all direct subheadings of this heading
  (org-show-children))


;; Hooks and Advices

(defun zc-org/narrow-to-subtree (&rest _)
  "Expand the headline and narrow to current subtree.

This function can be called after org jumping to a location:
```
;; Narrow to headline after jump, which affects:
;; - `counsel-org-goto'
;; - `counsel-org-goto-all'
(advice-add 'org-goto-marker-or-bmk :after #'zc-org/narrow-to-subtree)
'''"
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
                       (org-agenda-get-represented-categories)
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

(cl-defun zc-org/babel-foreach-block (fn &key pattern matcher)
  "Run FN for each source block in buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward pattern nil t)
        (let ((element (org-element-at-point)))
          (when (funcall matcher (org-element-type element))
            (funcall fn element)))))))

(defun zc-org/babel-remove-result-all ()
  "Remove results from every code block in buffer."
  (interactive)
  (zc-org/babel-foreach-block 'org-babel-remove-result-one-or-many
                              :pattern org-babel-src-block-regexp
                              :matcher (-partial 'eq 'src-block))
  (zc-org/babel-foreach-block '(lambda (&rest _) (org-babel-remove-result))
                              :pattern "^[ \t]*#\\+CALL:[ \t]*"
                              :matcher (-partial 'eq 'babel-call))
  (save-buffer))

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

(defun zc-org/babel-after-execute ()
  "Post-process babel execution result.

Function for `org-babel-after-execute-hook'."
  (interactive)
  (-when-let* ((location (org-babel-where-is-src-block-result))
               (info     (org-babel-get-src-block-info t))
               (case-fold-search t))
    (save-excursion
      (goto-char location)
      (when (looking-at org-babel-result-regexp)
        ;; Resize table columns width for the result of the
        ;; current babel source block.
        (save-excursion
          (forward-line)
          (when (org-at-table-p) (org-table-shrink)))))))


;; Smartparens

(defun zc-org/sp-point-in-checkbox-p (_id action _context)
  (and (eq action 'insert)
       (sp--looking-at-p "\\s-*]")))

(defun zc-org/sp-point-at-bol-p (_id action _context)
  (and (eq action 'insert)
       (eq (char-before) ?*)
       (sp--looking-back-p "^\\**" (line-beginning-position))))

(defun zc-org/sp-point-in-src-block-p (_id action _context)
  (and (eq action 'insert)
       (org-in-src-block-p nil)))



(provide 'zc-org-funcs)
