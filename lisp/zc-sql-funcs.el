(require 'dash)



(defun zc-sql/format-region-or-buffer (beg end)
  "Format SQL for the entire buffer or the marked region
between BEG and END.

Available SQL formatters:
- https://github.com/andialbrecht/sqlparse
- https://github.com/darold/pgFormatter
"
  (interactive "r")
  (unless (use-region-p) (setq beg (point-min)
                               end (point-max)))
  (save-excursion
    (shell-command-on-region beg end "sqlformat -r -k upper -" nil t)))



(provide 'zc-sql-funcs)
