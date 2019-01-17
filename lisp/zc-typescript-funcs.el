(require 'subr-x)

(autoload 'sp--looking-at-p "smartparens")
(autoload 'sp--looking-back-p "smartparens")
(autoload 'sp-end-of-sexp "smartparens")
(autoload 'sp-backward-whitespace "smartparens")
(autoload 'tide-net-sentinel "tide")

(defvar tide-servers)


;; Smartparens

(defun zc-typescript/sp-comment-expand (&rest _ignored)
  "Expand Javascript comment block."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (insert "*"))
  (insert "* ")
  (save-excursion
    (insert "\n")
    (indent-according-to-mode))
  (indent-according-to-mode))

(defun zc-typescript/sp-jsx-expand-tag (id action _context)
  "Expand JSX tag <> to self-closing form </> if point is not after a word."
  (when (and (eq action 'insert)
             (not (sp--looking-back-p
                   (concat "\\(\\sw\\|\\s_\\)" (regexp-quote id)))))
    (save-excursion (insert "/"))))

(defun zc-typescript/sp-jsx-rewrap-tag (&rest _ignored)
  "Rewrap the self-closing JSX tag <_/> to <_>|</_> if point is followed by />."
  (interactive "P")
  (if (sp--looking-at-p "/>")
      (let ((tag (zc-typescript/sp-jsx-get-tag-name))
            (beg (save-excursion (sp-backward-whitespace))))
        (delete-region beg (re-search-forward ">"))
        (insert ">\n")
        (save-excursion
          (insert "\n</" tag ">")
          (indent-according-to-mode))
        (indent-according-to-mode))
    (self-insert-command
     (prefix-numeric-value current-prefix-arg))))

(defun zc-typescript/sp-jsx-get-tag-name (&rest _ignored)
  "Return the JSX tag name inclosed in <> pair."
  (let* ((matched (save-excursion (sp-end-of-sexp)))
         (beg (plist-get matched :beg))
         (end (plist-get matched :end))
         (str (buffer-substring beg end))
         (sub (replace-regexp-in-string "/\\|<\\|>" "" str)))
    (string-trim (car (split-string sub " ")))))


;; Tide

(defun zc-typescript/tide-stop-all-servers ()
  "Kill all tide process sentinels and cleanup projects."
  (interactive)
  (maphash
   (lambda (project process)
     (tide-net-sentinel process "done"))
   tide-servers))

(defun zc-typescript/linter-fix-file ()
  (interactive)
  (shell-command (concat "tslint --fix " (buffer-file-name)))
  (revert-buffer t t))



(provide 'zc-typescript-funcs)
