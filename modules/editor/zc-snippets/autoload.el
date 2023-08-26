;;; editor/zc-snippets/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zc-yas/line-match-p (REGEXP)
  "Return t if line matches the given pattern."
  (string-match-p REGEXP (thing-at-point 'line t)))

;;;###autoload
(defun zc-yas/buffer-match-p (REGEXP)
  "Return t if buffer matches the given pattern."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward-regexp REGEXP nil t))))

;;;###autoload
(defun zc-yas/point-in-angle-pair-p ()
  "Return t if point is inside <>, nil otherwise."
  (equal "<" (plist-get (sp-get-enclosing-sexp) :op)))

;;;###autoload
(defun zc-yas/point-after-whitespace-p ()
  "Return t if point is after space, tab or newline, nil otherwise."
  (looking-back "[\s\n\t]+" 1))

;;;###autoload
(defun zc-yas/key-after-whitespace-p (key)
  "Return t if KEY is after space, tab or newline, nil otherwise."
  (looking-back (concat "[\s\n\t]+" (regexp-quote key)) 1))


;; Typescript

;;;###autoload
(defun zc-yas/react-buffer-p ()
  "Return t if buffer name ends with tsx or jsx."
  (let ((ext (file-name-extension (buffer-name))))
    (string-match-p "jsx\\|tsx" ext)))

;;;###autoload
(defun zc-yas/react-story-buffer-p ()
  "Return t if buffer name ends with .story.(tsx|jsx)."
  (string-match-p "\\.story.\\(jsx\\|tsx\\)\\'" (buffer-name)))

;;;###autoload
(defun zc-yas/test-buffer-p ()
  "Return t if buffer name contains .(test|spec). infix."
  (string-match-p ".\\(test\\|spec\\)." (buffer-name)))

;;;###autoload
(defun zc-yas/react-jsx-expand-prop-p (key)
  (and (zc-yas/react-buffer-p)
       (zc-yas/key-after-whitespace-p key)
       (zc-yas/point-in-angle-pair-p)))

;;;###autoload
(defun zc-yas/react-jsx-expand-tag-p (key)
  (and (zc-yas/react-buffer-p)
       (zc-yas/key-after-whitespace-p key)))
