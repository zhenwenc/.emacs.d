

(defun zc-sql/format-region (beg end)
  "Format SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "pg_format" nil t)))

(defun zc-sql/format-buffer ()
  "Format SQL in buffer."
  (interactive)
  (zc-sql/format-region (point-min) (point-max)))

(defun zc-sql/format-region-or-buffer (beg end)
  "Format SQL for the entire buffer or the marked region
between beg and end."
  (interactive "r")
  (if (use-region-p)
      (zc-sql/format-region beg end)
    (zc-sql/format-buffer)))



(provide 'zc-sql-funcs)
