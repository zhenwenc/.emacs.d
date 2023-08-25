(require 'dash)



(defun zc-sql/format-region-or-buffer (&optional beg end)
  "Format SQL for the entire buffer or the marked region
between BEG and END.

Available SQL formatters:
- https://github.com/andialbrecht/sqlparse
- https://github.com/darold/pgFormatter

Alternatives:
- https://github.com/purcell/sqlformat
- https://github.com/purcell/reformatter.el
"
  (interactive)
  (if (use-region-p)
      (setq beg (region-beginning)
            end (region-end))
    ;; Format region between comments.
    (setq beg (save-excursion
                (backward-paragraph)
                (skip-syntax-forward " >")
                (point))
          end (save-excursion
                (forward-paragraph)
                (skip-syntax-backward " >")
                (point)))
    ;; Format whole region if no comment found.
    (when (equal beg end)
      (setq beg (save-excursion (point-min))
            end (save-excursion (point-max)))))
  (save-excursion
    (shell-command-on-region beg end "sqlformat -r -k upper -" nil t)))



(provide 'zc-sql-funcs)
