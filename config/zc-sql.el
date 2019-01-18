;; We use `pg_format'(https://github.com/darold/pgFormatter)
;; to prettify the SQL queries.
;;
;; Install the package with:
;;   brew install pgFormatter

(eval-when-compile
  (require 'use-package))

(require 'zc-sql-funcs)
(require 'zc-hydra-funcs)

(autoload 'sql-set-product-feature "sql")



(use-package sql-mode
  :defer t
  :config
  (progn
    ;; PostgreSQL databases with underscores in their names
    ;; trip up the prompt specified in `sql.el'.
    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp
                             "^[-[:alnum:]_]*[-(][#>] ")

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*SQL*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.5)))))



(zc-hydra/major-mode-define sql-mode
  ("Basic"
   ()

   "Refactor"
   (("rf" zc-sql/format-region-or-buffer "format"))))



(provide 'zc-sql)
